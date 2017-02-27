import os
import sys

sys.setrecursionlimit(1000000)

from rsqueakvm import constants, wrapper, objspace, interpreter_bytecodes
from rsqueakvm.error import FatalError, Exit, SmalltalkException
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod, W_SpurCompiledMethod
from rsqueakvm.model.numeric import W_SmallInteger
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.storage_contexts import ContextPartShadow, ActiveContext, InactiveContext, DirtyContext

from rpython.rlib import jit, rstackovf, unroll, objectmodel, rsignal
from rpython.rlib.rarithmetic import ovfcheck


class ReturnFromTopLevel(Exception):
    _attrs_ = ["object", "s_current_frame"]
    def __init__(self, object, s_current_frame):
        self.object = object
        self.s_current_frame = s_current_frame

class Return(Exception):
    _attrs_ = []
    def value(self, space): raise NotImplementedError

class FreshReturn(Exception):
    _attrs_ = ["exception"]
    _immutable_fields_ = ["exception"]
    def __init__(self, exception):
        self.exception = exception

class LocalReturn(Return):
    _attrs_ = []
    @staticmethod
    def make(space, w_value):
        if isinstance(w_value, W_SmallInteger):
            return IntLocalReturn(space.unwrap_int(w_value))
        else:
            return WrappedLocalReturn(w_value)

class NonLocalReturn(Return):
    _attrs_ = ["s_home_context", "arrived_at_target"]
    _immutable_fields_ = ["s_target_context"]

    @staticmethod
    def make(space, s_home_context, w_value):
        if isinstance(w_value, W_SmallInteger):
            return IntNonLocalReturn(
                    s_home_context, space.unwrap_int(w_value))
        else:
            return WrappedNonLocalReturn(s_home_context, w_value)

    def __init__(self, s_home_context):
        self.s_home_context = s_home_context
        self.arrived_at_target = False

class WrappedLocalReturn(LocalReturn):
    _attrs_ = ["w_value"]
    _immutable_fields_ = ["w_value"]
    def __init__(self, w_result):
        self.w_value = w_result
    def value(self, space): return self.w_value

class IntLocalReturn(LocalReturn):
    _attrs_ = ["_value"]
    _immutable_fields_ = ["_value"]
    def __init__(self, intresult):
        self._value = intresult
    def value(self, space): return space.wrap_smallint_unsafe(self._value)

class WrappedNonLocalReturn(NonLocalReturn):
    _attrs_ = ["w_value"]
    _immutable_fields_ = ["w_value"]
    def __init__(self, s_home_context, w_value):
        NonLocalReturn.__init__(self, s_home_context)
        self.w_value = w_value
    def value(self, space): return self.w_value

class IntNonLocalReturn(NonLocalReturn):
    _attrs_ = ["_value"]
    _immutable_fields_ = ["_value"]
    def __init__(self, s_home_context, intvalue):
        NonLocalReturn.__init__(self, s_home_context)
        self._value = intvalue
    def value(self, space): return space.wrap_smallint_unsafe(self._value)

class NonVirtualReturn(Exception):
    _attrs_ = ["s_target_context", "s_current_context", "w_value"]
    def __init__(self, s_target_context, s_current_context, w_result):
        self.w_value = w_result
        self.s_target_context = s_target_context
        self.s_current_context = s_current_context

    def print_trace(self):
        print "\n====== Sender Chain Manipulation, contexts forced to heap at: %s" % self.s_current_context.short_str()

class ContextSwitchException(Exception):
    """General Exception that causes the interpreter to leave
    the current context."""
    _attrs_ = ["s_new_context"]
    type = "ContextSwitch"
    def __init__(self, s_new_context):
        self.s_new_context = s_new_context

    def print_trace(self):
        print "\n====== %s at: %s" % (self.type, self.s_new_context.short_str())

class StackOverflow(ContextSwitchException):
    """
    This causes the current jit-loop to be left, dumping all virtualized
    objects to the heap. This breaks performance, so it should rarely happen.
    In case of severe performance problems, execute with -t and check if this
    occurrs.
    """
    type = "Stack Overflow"

class ProcessSwitch(ContextSwitchException):
    """This causes the interpreter to switch the executed context.
    Triggered when switching the process."""
    _attrs_ = ["forced"]
    type = "Process Switch"
    def __init__(self, s_new_context, forced=False):
        ContextSwitchException.__init__(self, s_new_context)
        self.forced = forced

UNROLLING_BYTECODE_RANGES = unroll.unrolling_iterable(interpreter_bytecodes.BYTECODE_RANGES)

def get_printable_location(pc, self, method, w_class, blockmethod):
    bc = ord(method.bytes[pc])
    name = method.safe_identifier_string()
    classname = "???"
    if isinstance(w_class, W_PointersObject):
        s_class = w_class.strategy
        from rsqueakvm.storage_classes import ClassShadow
        if isinstance(s_class, ClassShadow):
            classname = s_class.getname()
    if blockmethod is None:
        return '%s(%s) [%d]: <%s>%s' % (classname, name, pc, hex(bc), interpreter_bytecodes.BYTECODE_NAMES[bc])
    else:
        blockname = blockmethod.safe_identifier_string()
        return '%s(%s): (%s) [%d]: <%s>%s' % (classname, name, blockname, pc, hex(bc), interpreter_bytecodes.BYTECODE_NAMES[bc])

def resume_get_printable_location(pc, self, method, w_class):
    return "resume: %s" % get_printable_location(pc, self, method, w_class, None)

# def confirm_enter_jit(pc, self, method, w_class, s_context):
#     print get_printable_location(pc, self, method, w_class)
#     return False

USE_SIGUSR1 = hasattr(rsignal, 'SIGUSR1')

jit_driver_name = "rsqueakjit"

class Interpreter(object):
    _immutable_fields_ = ["space",
                          "image",
                          "startup_time",
                          "evented",
                          "interrupts",
                          "trace_important",
                          "trace"]

    jit_driver = jit.JitDriver(
        name=jit_driver_name,
        greens=['pc', 'self', 'method', 'w_class', 'blockmethod'],
        reds=['s_context'],
        virtualizables=['s_context'],
        get_printable_location=get_printable_location,
        is_recursive=True
    )

    resume_driver = jit.JitDriver(
        name=jit_driver_name + "_resume",
        greens=['pc', 'self', 'method', 'w_class'],
        reds=['s_context'],
        # virtualizables=['s_context'],
        get_printable_location=resume_get_printable_location,
        is_recursive=True
    )

    def __init__(self, space, image=None, trace_important=False,
                 trace=False, evented=True, interrupts=True):
        # === Initialize immutable variables
        self.space = space
        self.image = image
        if image:
            self.startup_time = image.startup_time
        else:
            self.startup_time = constants.CompileTime
        self.evented = evented
        self.interrupts = interrupts
        self.trace_important = trace_important
        try:
            self.interrupt_counter_size = int(os.environ["SPY_ICS"])
        except KeyError:
            self.interrupt_counter_size = constants.INTERRUPT_COUNTER_SIZE
        self.last_check = self.time_now()
        self.trace = trace

        # === Initialize mutable variables
        self.interrupt_check_counter = self.interrupt_counter_size
        self.next_wakeup_tick = 0
        self.stack_depth = 0
        self.process_switch_count = 0
        self.forced_interrupt_checks_count = 0
        self.stack_overflow_count = 0

        if not objectmodel.we_are_translated():
            if USE_SIGUSR1:
                rsignal.pypysig_setflag(rsignal.SIGUSR1)

    def loop(self, w_active_context):
        # This is the top-level loop and is not invoked recursively.
        s_context = w_active_context.as_context_get_shadow(self.space)
        while True:
            method = s_context.w_method()
            pc = s_context.pc()
            self.resume_driver.jit_merge_point(
                pc=pc,
                self=self,
                method=method,
                w_class=self.getreceiverclass(s_context),
                s_context=s_context)
            s_sender = s_context.s_sender()
            try:
                self.stack_frame(s_context, None)
                raise Exception("loop_bytecodes left without raising...")
            except ProcessSwitch, e:
                if self.is_tracing() or self.trace_important:
                    e.print_trace()
                self.process_switch_count += 1
                s_context = e.s_new_context
                if not e.forced:
                    method = s_context.w_method()
                    pc = s_context.pc()
                    self.resume_driver.can_enter_jit(
                        pc=pc,
                        self=self,
                        method=method,
                        w_class=self.getreceiverclass(s_context),
                        s_context=s_context)
            except StackOverflow, e:
                if self.is_tracing() or self.trace_important:
                    e.print_trace()
                self.stack_overflow_count += 1
                s_context = e.s_new_context
            except LocalReturn, ret:
                target = s_sender
                s_context = self.unwind_context_chain_local(target, ret.value(self.space), s_context)
                if self.is_tracing() or self.trace_important:
                    print "\n====== Local Return in top-level loop, contexts forced to heap at: %s" % s_context.short_str()
            except NonLocalReturn, ret:
                target = s_sender if ret.arrived_at_target else ret.s_home_context.s_sender() # fine to force here
                s_context = self.unwind_context_chain(s_sender, target, ret.value(self.space), s_context)
                if self.is_tracing() or self.trace_important:
                    print "\n====== Non Local Return in top-level loop, contexts forced to heap at: %s" % s_context.short_str()
            except NonVirtualReturn, ret:
                if self.is_tracing() or self.trace_important:
                    ret.print_trace()
                s_context = self.unwind_context_chain(ret.s_current_context, ret.s_target_context, ret.w_value, s_context)

    # This is a wrapper around loop_bytecodes that cleanly enters/leaves the frame,
    # handles the stack overflow protection mechanism and handles/dispatches Returns.
    def stack_frame(self, s_frame, s_sender, may_context_switch=True):
        if self.is_tracing():
            self.stack_depth += 1
        vref = s_frame.enter_virtual_frame(s_sender)
        try:
            self.loop_bytecodes(s_frame, may_context_switch)
        except rstackovf.StackOverflow:
            rstackovf.check_stack_overflow()
            raise StackOverflow(s_frame)
        except LocalReturn, ret:
            if s_frame.get_state() is DirtyContext:
                s_new_sender = s_frame.s_sender()  # The sender has changed!
                s_frame._activate_unwind_context(self)
                raise NonVirtualReturn(s_new_sender, s_new_sender, ret.value(self.space))
            else:
                s_frame._activate_unwind_context(self)
                raise ret
        except NonLocalReturn, ret:
            if s_frame.get_state() is DirtyContext:
                s_new_sender = s_frame.s_sender()  # The sender has changed!
                # To get the target context:
                #  a) we can force the home_context.s_sender() here, we're spilling to the heap anyway
                #  b) we need to do it before we unwind the current context, because s_home and s_frame may be the same
                s_target_context = ret.s_home_context.s_sender()
                s_frame._activate_unwind_context(self)
                raise NonVirtualReturn(s_target_context, s_new_sender, ret.value(self.space))
            else:
                s_frame._activate_unwind_context(self)
                if ret.s_home_context is s_frame:
                    ret.arrived_at_target = True
                raise ret
        finally:
            if self.is_tracing():
                self.stack_depth -= 1
            s_frame.leave_virtual_frame(vref, s_sender)

    def getreceiverclass(self, s_context):
        return s_context.w_receiver().safe_getclass(self.space)

    def getblockmethod(self, s_context):
        return s_context.blockmethod

    def loop_bytecodes(self, s_context, may_context_switch=True):
        old_pc = 0
        if not jit.we_are_jitted() and may_context_switch:
            self.quick_check_for_interrupt(s_context)
        method = s_context.w_method()
        while True:
            pc = s_context.pc()
            if pc < old_pc:
                if jit.we_are_jitted():
                    # Do the interrupt-check at the end of a loop, don't interrupt loops midway.
                    self.jitted_check_for_interrupt(s_context)
                self.jit_driver.can_enter_jit(
                    pc=pc, self=self, method=method,
                    w_class=self.getreceiverclass(s_context),
                    blockmethod=self.getblockmethod(s_context),
                    s_context=s_context)
            old_pc = pc
            self.jit_driver.jit_merge_point(
                pc=pc, self=self, method=method,
                w_class=self.getreceiverclass(s_context),
                blockmethod=self.getblockmethod(s_context),
                s_context=s_context)
            try:
                self.step(s_context)
            except FreshReturn, ret:
                raise ret.exception
            except LocalReturn, ret:
                s_context.push(ret.value(self.space))
            except NonLocalReturn, ret:
                if ret.arrived_at_target:
                    s_context.push(ret.value(self.space))
                else:
                    raise ret

    def unwind_primitive_simulation(self, start_context, error_code):
        context = start_context
        while context.get_fallback() is None:
            s_sender = context.s_sender()
            context = s_sender
            if not context:
                msg = "Context chain ended while trying to unwind primitive simulation\nfrom\n%s\n(pc %s)" % (
                        start_context.short_str(),
                        start_context.pc())
                raise FatalError(msg)
        fallbackContext = context.get_fallback()
        fallbackContext.store_s_sender(context.s_sender())

        if fallbackContext.tempsize() > len(fallbackContext.w_arguments()):
            fallbackContext.settemp(len(fallbackContext.w_arguments()), self.space.wrap_int(error_code))

        return fallbackContext

    @jit.unroll_safe
    def unwind_context_chain(self, start_context, target_context, return_value,
                             s_current_context):
        if start_context is None:
            # This is the toplevel frame. Execution ended.
            raise ReturnFromTopLevel(return_value, s_current_context)
        assert target_context

        context = start_context
        for i in range(4):
            if context is target_context:
                break
            context = self._finish_context(context, target_context, return_value, s_current_context)

        if context is target_context:
            context.push(return_value)
            return context
        else:
            return self._unwind_context_chain(context, target_context, return_value, s_current_context)

    def _unwind_context_chain(self, start_context, target_context, return_value,
                             s_current_context):
        context = start_context
        while context is not target_context:
            context = self._finish_context(context, target_context, return_value, s_current_context)
        context.push(return_value)
        return context

    def _finish_context(self, context, target_context, return_value, s_current_context):
        if not context:
            msg = "Context chain ended while trying to return\n%s\nfrom somewhere near or above or below\n%s\n(pc %s)\nto\n%s\n(pc %s)" % (
                    return_value.as_repr_string(),
                    context.short_str(),
                    context.pc(),
                    target_context.short_str(),
                    target_context.pc())
            raise FatalError(msg)
        s_sender = context.s_sender()
        context._activate_unwind_context(self)
        return s_sender

    def unwind_context_chain_local(self, target_context, return_value, s_current_context):
        if target_context is None:
            # This is the toplevel frame. Execution ended.
            raise ReturnFromTopLevel(return_value, s_current_context)
        target_context.push(return_value)
        return target_context

    def step(self, context):
        if not objectmodel.we_are_translated():
            if USE_SIGUSR1:
                self.check_sigusr(context)

        bytecode = context.fetch_next_bytecode()
        for entry in UNROLLING_BYTECODE_RANGES:
            if len(entry) == 2:
                bc, methname = entry
                if bytecode == bc:
                    return getattr(context, methname)(self, bytecode)
            else:
                start, stop, methname = entry
                if start <= bytecode <= stop:
                    return getattr(context, methname)(self, bytecode)
        assert 0, "unreachable"

    # ============== Methods for handling user interrupts ==============

    def jitted_check_for_interrupt(self, s_frame):
        if not self.interrupts:
            return
        # Normally, the tick counter is decremented by 1 for every message send.
        # Since we don't know how many messages are called during this trace, we
        # just divide by 12**2 and make sure it's always at least 1
        trace_length = jit.current_trace_length()
        decr_by = int(trace_length >> 12 | 1)
        self.quick_check_for_interrupt(s_frame, decr_by)

    def quick_check_for_interrupt(self, s_frame, dec=1):
        if not self.interrupts:
            return
        self.interrupt_check_counter -= dec
        if self.interrupt_check_counter <= 0:
            self.interrupt_check_counter = self.interrupt_counter_size
            self.check_for_interrupts(s_frame)

    def check_sigusr(self, s_frame):
        poll = rsignal.pypysig_poll()
        if poll == rsignal.SIGUSR1:
            print s_frame.print_stack()

    def signal_memory_error(self, s_frame):
        w_low_space_sema = self.space.w_low_space_semaphore()
        if w_low_space_sema is not self.space.w_nil:
            assert isinstance(w_low_space_sema, W_PointersObject)
            wrapper.SemaphoreWrapper(self.space, w_low_space_sema).signal(s_frame, forced=True)

    def check_for_interrupts(self, s_frame):
        display = self.space.display()
        if display:
            display.render()
        # parallel to Interpreter>>#checkForInterrupts
        # 1. profiling is done using rvmprof
        # 2. use the same time value as the primitive UTC_MICROSECOND_CLOCK
        now = self.time_now()
        # 3. adjust the check counter size, we want to land between 100ms and 400ms
        diff = now - self.last_check
        if diff < 100000 and self.interrupt_counter_size != constants.MAXINT:
            try:
                self.interrupt_counter_size = ovfcheck(self.interrupt_counter_size * 2)
            except OverflowError:
                self.interrupt_counter_size = constants.MAXINT
        elif diff > 400000 and self.interrupt_counter_size > 100:
            self.interrupt_counter_size = max(self.interrupt_counter_size / 2, 100)
        self.last_check = now
        self.forced_interrupt_checks_count += 1

        # 4. check for User Interrupt
        if self.space.display().has_interrupts_pending():
            w_interrupt_sema = self.space.w_interrupt_semaphore()
            if w_interrupt_sema is not self.space.w_nil:
                assert isinstance(w_interrupt_sema, W_PointersObject)
                wrapper.SemaphoreWrapper(self.space, w_interrupt_sema).signal(s_frame, forced=True)

        # 5. the low space semaphore is signalled in ClassShadow#new
        # 6. signal the timer
        if not self.next_wakeup_tick == 0 and now >= self.next_wakeup_tick:
            self.next_wakeup_tick = 0
            semaphore = self.space.w_timerSemaphore()
            if not semaphore.is_nil(self.space):
                assert isinstance(semaphore, W_PointersObject)
                wrapper.SemaphoreWrapper(self.space, semaphore).signal(s_frame, forced=False)
        # We have no finalization process, so far.
        # We do not support external semaphores.
        # In cog, the method to add such a semaphore is only called in GC.

    def time_now(self):
        """
        Answer the UTC microseconds since the Smalltalk epoch. The value is
        derived from the Posix epoch with a constant offset corresponding to
        elapsed microseconds between the two epochs according to RFC 868
        """
        import time
        from rpython.rlib.rarithmetic import r_int64
        secs_to_usecs = 1000 * 1000
        return r_int64(time.time() * secs_to_usecs) + constants.SQUEAK_EPOCH_DELTA_MICROSECONDS

    def event_time_now(self):
        """
        Answer the number of milliseconds since the millisecond clock was last
        reset or rolled over.
        """
        import time
        from rpython.rlib.rarithmetic import intmask
        return intmask(int((time.time() - self.startup_time) * 1000) & constants.TAGGED_MASK)

    # ============== Convenience methods for executing code ==============

    def interpret_toplevel(self, w_frame):
        try:
            self.interrupt_check_counter = self.interrupt_counter_size
            self.loop(w_frame)
        except ReturnFromTopLevel, e:
            if not self.space.headless.is_set():
                w_cannotReturn = self.space.w_cannotReturn
                if w_cannotReturn is not None:
                    s_context = self.create_toplevel_context(
                        e.s_current_frame.w_self(),
                        w_selector=w_cannotReturn,
                        w_arguments=[e.object])
                    return self.loop(s_context.w_self())
            return e.object

    def perform_headless(self, w_receiver, w_selector, w_arguments):
        with objspace.ForceHeadless(self.space):
            try:
                return self.perform(w_receiver, w_selector=w_selector, w_arguments=w_arguments)
            except Exit, SmalltalkException:
                pass
        return w_receiver

    def perform(self, w_receiver, selector="", w_selector=None, w_arguments=[]):
        s_frame = self.create_toplevel_context(w_receiver, selector, w_selector, w_arguments)
        return self.interpret_toplevel(s_frame.w_self())

    def create_toplevel_context(self, w_receiver, selector="", w_selector=None, w_arguments=[]):
        if w_selector is None:
            assert selector, "Need either string or W_Object selector"
            if selector == "asSymbol":
                w_selector = self.image.w_asSymbol
            else:
                with objspace.ForceHeadless(self.space):
                    w_selector = self.perform(self.space.wrap_string(selector),
                                              "asSymbol")

        if self.space.is_spur.is_set():
            w_method = W_SpurCompiledMethod(self.space, header=512)
        else:
            w_method = W_PreSpurCompiledMethod(self.space, header=512)
        w_method.literalatput0(self.space, 1, w_selector)
        assert len(w_arguments) <= 7
        w_method.setbytes([chr(131), chr(len(w_arguments) << 5 + 0), chr(124)])  #returnTopFromMethodBytecode
        w_method.set_lookup_class_and_name(w_receiver.getclass(self.space),
                                           "Interpreter.perform")
        s_frame = ContextPartShadow.build_method_context(self.space, w_method,
                                                         w_receiver)
        s_frame.push(w_receiver)
        s_frame.push_all(list(w_arguments))
        return s_frame

    # ============== Methods for tracing and printing ==============

    def is_tracing(self):
        return jit.promote(self.trace)

    def print_padded(self, str):
        assert self.is_tracing()
        print (' ' * self.stack_depth) + str


# Uncomment this to load debugging facilities at startup.
#from rsqueakvm import interpreter_debugging; Interpreter.__init__ = interpreter_debugging.activating_init(Interpreter.__init__)
