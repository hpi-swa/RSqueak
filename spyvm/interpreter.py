import os
import sys

sys.setrecursionlimit(1000000)

from spyvm.storage_contexts import ContextPartShadow, ActiveContext, InactiveContext, DirtyContext
from spyvm import model, constants, wrapper, objspace, interpreter_bytecodes, error
from spyvm.error import MetaPrimFailed

from rpython.rlib import jit, rstackovf, unroll, objectmodel, rsignal



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
        if isinstance(w_value, model.W_SmallInteger):
            return IntLocalReturn(space.unwrap_int(w_value))
        else:
            return WrappedLocalReturn(w_value)

class NonLocalReturn(Return):
    _attrs_ = ["s_target_context", "arrived_at_target"]
    _immutable_fields_ = ["s_target_context"]

    @staticmethod
    def make(space, s_target_context, w_value):
        if isinstance(w_value, model.W_SmallInteger):
            return IntNonLocalReturn(s_target_context, space.unwrap_int(w_value))
        else:
            return WrappedNonLocalReturn(s_target_context, w_value)

    def __init__(self, s_target_context):
        self.s_target_context = s_target_context
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
    def value(self, space): return space.wrap_int(self._value)

class WrappedNonLocalReturn(NonLocalReturn):
    _attrs_ = ["w_value"]
    _immutable_fields_ = ["w_value"]
    def __init__(self, s_target_context, w_value):
        NonLocalReturn.__init__(self, s_target_context)
        self.w_value = w_value
    def value(self, space): return self.w_value

class IntNonLocalReturn(NonLocalReturn):
    _attrs_ = ["_value"]
    _immutable_fields_ = ["_value"]
    def __init__(self, s_target_context, intvalue):
        NonLocalReturn.__init__(self, s_target_context)
        self._value = intvalue
    def value(self, space): return space.wrap_int(self._value)

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
    """This causes the current jit-loop to be left, dumping all virtualized objects to the heap.
    This breaks performance, so it should rarely happen.
    In case of severe performance problems, execute with -t and check if this occurrs."""
    type = "Stack Overflow"

class ProcessSwitch(ContextSwitchException):
    """This causes the interpreter to switch the executed context.
    Triggered when switching the process."""
    type = "Process Switch"

UNROLLING_BYTECODE_RANGES = unroll.unrolling_iterable(interpreter_bytecodes.BYTECODE_RANGES)

def get_printable_location(pc, self, method):
    bc = ord(method.bytes[pc])
    name = method.safe_identifier_string()
    return '(%s) [%d]: <%s>%s' % (name, pc, hex(bc), interpreter_bytecodes.BYTECODE_NAMES[bc])

USE_SIGUSR1 = hasattr(rsignal, 'SIGUSR1')


class Interpreter(object):
    _immutable_fields_ = ["space",
                          "image",
                          "startup_time",
                          "evented",
                          "interrupts",
                          "trace_important",
                          "interrupt_counter_size",
                          "trace"]

    jit_driver = jit.JitDriver(
        greens=['pc', 'self', 'method'],
        reds=['s_context'],
        virtualizables=['s_context'],
        get_printable_location=get_printable_location
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
        self.trace = trace

        # === Initialize mutable variables
        self.interrupt_check_counter = self.interrupt_counter_size
        self.next_wakeup_tick = 0
        self.trace_proxy = objspace.ConstantFlag()
        self.stack_depth = 0

        if not objectmodel.we_are_translated():
            if USE_SIGUSR1:
                rsignal.pypysig_setflag(rsignal.SIGUSR1)

    def populate_remaining_special_objects(self):
        with objspace.ForceHeadless(self.space):
            for name, idx in constants.objects_in_special_object_table.items():
                name = "w_" + name
                if name not in self.space.objtable or not self.space.objtable[name]:
                    if name == "w_runWithIn":
                        w_string = self.space.wrap_string("run:with:in:")
                        self.space.objtable[name] = self.perform(w_string, selector="asSymbol")
                        assert self.space.objtable[name]
                    else:
                        raise Warning("don't know how to populate " + name + " which was not in special objects table")

    def loop(self, w_active_context):
        # This is the top-level loop and is not invoked recursively.
        s_context = w_active_context.as_context_get_shadow(self.space)
        while True:
            s_sender = s_context.s_sender()
            try:
                self.stack_frame(s_context, None)
                raise Exception("loop_bytecodes left without raising...")
            except ContextSwitchException, e:
                if self.is_tracing() or self.trace_important:
                    e.print_trace()
                s_context = e.s_new_context
            except LocalReturn, ret:
                target = s_sender
                s_context = self.unwind_context_chain(s_sender, target, ret.value(self.space), s_context)
            except NonLocalReturn, ret:
                target = s_sender if ret.arrived_at_target else ret.s_target_context
                s_context = self.unwind_context_chain(s_sender, target, ret.value(self.space), s_context)
            except NonVirtualReturn, ret:
                if self.is_tracing() or self.trace_important:
                    ret.print_trace()
                s_context = self.unwind_context_chain(ret.s_current_context, ret.s_target_context, ret.w_value, s_context)
            except MetaPrimFailed, e:
                s_context = self.unwind_primitive_simulation(e.s_frame, e.error_code, s_context)

    # This is a wrapper around loop_bytecodes that cleanly enters/leaves the frame,
    # handles the stack overflow protection mechanism and handles/dispatches Returns.
    def stack_frame(self, s_frame, s_sender, may_context_switch=True):
        try:
            if self.is_tracing():
                self.stack_depth += 1
            if s_frame._s_sender is None and s_sender is not None:
                s_frame.store_s_sender(s_sender)
            # Now (continue to) execute the context bytecodes
            # assert s_frame.state is InactiveContext
            s_frame.state = ActiveContext
            self.loop_bytecodes(s_frame, may_context_switch)
        except rstackovf.StackOverflow:
            rstackovf.check_stack_overflow()
            raise StackOverflow(s_frame)
        except LocalReturn, ret:
            if s_frame.state is DirtyContext:
                s_sender = s_frame.s_sender() # The sender has changed!
                s_frame._activate_unwind_context(self)
                raise NonVirtualReturn(s_sender, s_sender, ret.value(self.space))
            else:
                s_frame._activate_unwind_context(self)
                raise ret
        except NonLocalReturn, ret:
            if s_frame.state is DirtyContext:
                s_sender = s_frame.s_sender() # The sender has changed!
                s_frame._activate_unwind_context(self)
                raise NonVirtualReturn(ret.s_target_context, s_sender, ret.value(self.space))
            else:
                s_frame._activate_unwind_context(self)
                if ret.s_target_context is s_sender:
                    ret.arrived_at_target = True
                raise ret
        finally:
            if self.is_tracing():
                self.stack_depth -= 1
            s_frame.state = InactiveContext

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
                    s_context=s_context)
            old_pc = pc
            self.jit_driver.jit_merge_point(
                pc=pc, self=self, method=method,
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

    def unwind_primitive_simulation(self, start_context, error_code, s_current_context):
        if start_context is None:
            # This is the toplevel frame. Execution ended.
            raise ReturnFromTopLevel(self.space.w_nil, s_current_context)
        context = start_context
        while context.get_fallback() is None:
            s_sender = context.s_sender()
            context._activate_unwind_context(self)
            context = s_sender

            if not context:
                msg = "Context chain ended while trying to unwind primitive simulation\nfrom\n%s\n(pc %s)" % (
                        start_context.short_str(),
                        start_context.pc())
                raise error.FatalError(msg)

        fallbackContext = context.get_fallback()

        if fallbackContext.tempsize() > len(fallbackContext.w_arguments()):
            fallbackContext.settemp(len(fallbackContext.w_arguments()), self.space.wrap_int(error_code))

        return fallbackContext

    def unwind_context_chain(self, start_context, target_context, return_value, s_current_context):
        if start_context is None:
            # This is the toplevel frame. Execution ended.
            raise ReturnFromTopLevel(return_value, s_current_context)
        assert target_context
        context = start_context
        while context is not target_context:
            if not context:
                msg = "Context chain ended while trying to return\n%s\nfrom\n%s\n(pc %s)\nto\n%s\n(pc %s)" % (
                        return_value.as_repr_string(),
                        start_context.short_str(),
                        start_context.pc(),
                        target_context.short_str(),
                        start_context.pc())
                raise error.FatalError(msg)
            s_sender = context.s_sender()
            context._activate_unwind_context(self)
            context = s_sender
        context.push(return_value)
        return context

    def step(self, context):
        if not objectmodel.we_are_translated():
            if USE_SIGUSR1: self.check_sigusr(context)


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
        # just decrement by 1000th of the trace length (num of bytecodes).
        trace_length = jit.current_trace_length()
        decr_by = int(trace_length // 1000)
        if decr_by > 0:
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

    def check_for_interrupts(self, s_frame):
        # parallel to Interpreter>>#checkForInterrupts

        # Profiling is skipped
        # We don't adjust the check counter size

        # use the same time value as the primitive UTC_MICROSECOND_CLOCK
        now = self.time_now()

        # XXX the low space semaphore may be signaled here
        # Process inputs
        # Process User Interrupt?
        if not self.next_wakeup_tick == 0 and now >= self.next_wakeup_tick:
            self.next_wakeup_tick = 0
            semaphore = self.space.objtable["w_timerSemaphore"]
            if not semaphore.is_nil(self.space):
                wrapper.SemaphoreWrapper(self.space, semaphore).signal(s_frame)
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
        from rpython.rlib.rarithmetic import r_longlong
        secs_to_usecs = 1000 * 1000
        return r_longlong(time.time() * secs_to_usecs) + constants.SQUEAK_EPOCH_DELTA_MICROSECONDS

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
                w_cannotReturn = self.space.special_object("w_cannotReturn")
                if w_cannotReturn is not None:
                    s_context = self.create_toplevel_context(
                        e.s_current_frame.w_self(),
                        w_selector=w_cannotReturn,
                        w_arguments=[e.object])
                    return self.loop(s_context.w_self())
            return e.object

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
                    w_selector = self.perform(self.space.wrap_string(selector), "asSymbol")

        if self.space.is_spur.is_set():
            w_method = model.W_SpurCompiledMethod(self.space, header=512)
        else:
            w_method = model.W_PreSpurCompiledMethod(self.space, header=512)
        w_method.literalatput0(self.space, 1, w_selector)
        assert len(w_arguments) <= 7
        w_method.setbytes([chr(131), chr(len(w_arguments) << 5 + 0), chr(124)]) #returnTopFromMethodBytecode
        w_method.set_lookup_class_and_name(w_receiver.getclass(self.space), "Interpreter.perform")
        s_frame = ContextPartShadow.build_method_context(self.space, w_method, w_receiver)
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
#from spyvm import interpreter_debugging; Interpreter.__init__ = interpreter_debugging.activating_init(Interpreter.__init__)
