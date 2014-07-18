import py
import os
from spyvm.shadow import ContextPartShadow, MethodContextShadow, BlockContextShadow, MethodNotFound, ActiveContext, InactiveContext, DirtyContext
from spyvm import model, constants, primitives, conftest, wrapper, objspace
from spyvm.tool.bitmanipulation import splitter

from rpython.rlib import jit, rstackovf
from rpython.rlib import objectmodel, unroll

class MissingBytecode(Exception):
    """Bytecode not implemented yet."""
    def __init__(self, bytecodename):
        self.bytecodename = bytecodename
        print "MissingBytecode:", bytecodename     # hack for debugging

class IllegalStoreError(Exception):
    """Illegal Store."""

def get_printable_location(pc, self, method):
    bc = ord(method.bytes[pc])
    name = method.safe_identifier_string()
    return '(%s) [%d]: <%s>%s' % (name, pc, hex(bc), BYTECODE_NAMES[bc])


class Interpreter(object):
    _immutable_fields_ = ["space", "image", "image_name",
                          "interrupt_counter_size",
                          "startup_time", "evented", "interrupts"]

    jit_driver = jit.JitDriver(
        greens=['pc', 'self', 'method'],
        reds=['s_context'],
        virtualizables=['s_context'],
        get_printable_location=get_printable_location
    )

    def __init__(self, space, image=None, image_name="",
                trace=False, evented=True, interrupts=True):
        # === Initialize immutable variables
        self.space = space
        self.image = image
        self.image_name = image_name
        if image:
            self.startup_time = image.startup_time
        else:
            self.startup_time = constants.CompileTime
        self.evented = evented
        self.interrupts = interrupts
        try:
            self.interrupt_counter_size = int(os.environ["SPY_ICS"])
        except KeyError:
            self.interrupt_counter_size = constants.INTERRUPT_COUNTER_SIZE

        # === Initialize mutable variables
        self.interrupt_check_counter = self.interrupt_counter_size
        self.next_wakeup_tick = 0
        self.trace = trace
        self.trace_proxy = objspace.ConstantFlag()
        self.stack_depth = 0

    def loop(self, w_active_context):
        # This is the top-level loop and is not invoked recursively.
        s_new_context = w_active_context.as_context_get_shadow(self.space)
        while True:
            s_sender = s_new_context.s_sender()
            try:
                self.loop_bytecodes(s_new_context)
                raise Exception("loop_bytecodes left without raising...")
            except ContextSwitchException, e:
                if self.is_tracing():
                    e.print_trace(s_new_context)
                s_new_context = e.s_new_context
            except Return, nlr:
                assert nlr.s_target_context or nlr.is_local
                s_new_context = s_sender
                if not nlr.is_local:
                    while s_new_context is not nlr.s_target_context:
                        s_sender = s_new_context.s_sender()
                        s_new_context._activate_unwind_context(self)
                        s_new_context = s_sender
                s_new_context.push(nlr.value)
    
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
            except Return, nlr:
                if nlr.s_target_context is s_context or nlr.is_local:
                    s_context.push(nlr.value)
                else:
                    if nlr.s_target_context is None:
                        # This is the case where we are returning to our sender.
                        # Mark the return as local, so our sender will take it
                        nlr.is_local = True
                    s_context._activate_unwind_context(self)
                    raise nlr

    # This is a wrapper around loop_bytecodes that cleanly enters/leaves the frame
    # and handles the stack overflow protection mechanism.
    def stack_frame(self, s_frame, s_sender, may_context_switch=True):
        try:
            if self.is_tracing():
                self.stack_depth += 1
            if s_frame._s_sender is None and s_sender is not None:
                s_frame.store_s_sender(s_sender)
            # Now (continue to) execute the context bytecodes
            assert s_frame.state is InactiveContext
            s_frame.state = ActiveContext
            self.loop_bytecodes(s_frame, may_context_switch)
        except rstackovf.StackOverflow:
            rstackovf.check_stack_overflow()
            raise StackOverflow(s_frame)
        finally:
            if self.is_tracing():
                self.stack_depth -= 1
            dirty_frame = s_frame.state is DirtyContext
            s_frame.state = InactiveContext
            if dirty_frame:
                raise SenderChainManipulation(s_frame)
    
    def step(self, context):
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
        # just decrement by 100th of the trace length (num of bytecodes).
        trace_length = jit.current_trace_length()
        decr_by = int(trace_length // 100)
        decr_by = max(decr_by, 1)
        self.quick_check_for_interrupt(s_frame, decr_by)

    def quick_check_for_interrupt(self, s_frame, dec=1):
        if not self.interrupts:
            return
        self.interrupt_check_counter -= dec
        if self.interrupt_check_counter <= 0:
            self.interrupt_check_counter = self.interrupt_counter_size
            self.check_for_interrupts(s_frame)

    def check_for_interrupts(self, s_frame):
        # parallel to Interpreter>>#checkForInterrupts

        # Profiling is skipped
        # We don't adjust the check counter size

        # use the same time value as the primitive MILLISECOND_CLOCK
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
        import time
        from rpython.rlib.rarithmetic import intmask
        return intmask(int((time.time() - self.startup_time) * 1000) & constants.TAGGED_MASK)

    # ============== Convenience methods for executing code ==============

    def interpret_toplevel(self, w_frame):
        try:
            self.interrupt_check_counter = self.interrupt_counter_size
            self.loop(w_frame)
        except ReturnFromTopLevel, e:
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
                w_selector = self.perform(self.space.wrap_string(selector), "asSymbol")

        w_method = model.W_CompiledMethod(self.space, header=512)
        w_method.literalatput0(self.space, 1, w_selector)
        assert len(w_arguments) <= 7
        w_method.setbytes([chr(131), chr(len(w_arguments) << 5 + 0), chr(124)]) #returnTopFromMethodBytecode
        w_method.set_lookup_class_and_name(w_receiver.getclass(self.space), "Interpreter.perform")
        s_frame = MethodContextShadow(self.space, w_method=w_method, w_receiver=w_receiver)
        s_frame.push(w_receiver)
        s_frame.push_all(list(w_arguments))
        return s_frame
        
    # ============== Methods for tracing, printing and debugging ==============
    
    def is_tracing(self):
        return jit.promote(self.trace)
    
    def print_padded(self, str):
        assert self.is_tracing()
        print (' ' * self.stack_depth) + str
    
    def activate_debug_bytecode(self):
        "NOT_RPYTHON"
        def do_break(self):
            import pdb
            if self.break_on_bytecodes:
                pdb.set_trace()
        Interpreter.debug_bytecode = do_break
        self.break_on_bytecodes = True
    
    def debug_bytecode(self):
        # This is for debugging. In a pdb console, execute the following:
        # self.activate_debug_bytecode()
        pass

class ReturnFromTopLevel(Exception):
    _attrs_ = ["object"]
    def __init__(self, object):
        self.object = object

class Return(Exception):
    _attrs_ = ["value", "s_target_context", "is_local"]
    def __init__(self, s_target_context, w_result):
        self.value = w_result
        self.s_target_context = s_target_context
        self.is_local = False

class ContextSwitchException(Exception):
    """General Exception that causes the interpreter to leave
    the current context."""
    
    _attrs_ = ["s_new_context"]
    type = "ContextSwitch"
    def __init__(self, s_new_context):
        self.s_new_context = s_new_context
    
    def print_trace(self, old_context):
        print "====== %s, contexts forced to heap at: %s" % (self.type, self.s_new_context.short_str())
        
class StackOverflow(ContextSwitchException):
    """This causes the current jit-loop to be left, dumping all virtualized objects to the heap.
    This breaks performance, so it should rarely happen.
    In case of severe performance problems, execute with -t and check if this occurrs."""
    type = "Stack Overflow"
    
class ProcessSwitch(ContextSwitchException):
    """This causes the interpreter to switch the executed context.
    Triggered when switching the process."""
    
    def print_trace(self, old_context):
        print "====== Switched process from: %s" % old_context.short_str()
        print "====== to: %s " % self.s_new_context.short_str()
    
class SenderChainManipulation(ContextSwitchException):
    """Manipulation of the sender chain can invalidate the jitted C stack.
    We have to dump all virtual objects and rebuild the stack.
    We try to raise this as rarely as possible and as late as possible."""
    type = "Sender Manipulation"

import rpython.rlib.unroll
if hasattr(unroll, "unrolling_zero"):
    unrolling_zero = unroll.unrolling_zero
else:
    class unrolling_int(int, unroll.SpecTag):
        def __add__(self, other):
            return unrolling_int(int.__add__(self, other))
        __radd__ = __add__
        def __sub__(self, other):
            return unrolling_int(int.__sub__(self, other))
        def __rsub__(self, other):
            return unrolling_int(int.__rsub__(self, other))
    unrolling_zero = unrolling_int(0)


# This is a decorator for bytecode implementation methods.
# parameter_bytes=N means N additional bytes are fetched as parameters.
def bytecode_implementation(parameter_bytes=0):
    def bytecode_implementation_decorator(actual_implementation_method):
        @jit.unroll_safe
        def bytecode_implementation_wrapper(self, interp, current_bytecode):
            parameters = ()
            i = unrolling_zero
            while i < parameter_bytes:
                parameters += (self.fetch_next_bytecode(), )
                i = i + 1
            # This is a good place to step through bytecodes.
            interp.debug_bytecode()
            return actual_implementation_method(self, interp, current_bytecode, *parameters)
        bytecode_implementation_wrapper.func_name = actual_implementation_method.func_name
        return bytecode_implementation_wrapper
    return bytecode_implementation_decorator

def make_call_primitive_bytecode(primitive, selector, argcount, store_pc=False):
    func = primitives.prim_table[primitive]
    @bytecode_implementation()
    def callPrimitive(self, interp, current_bytecode):
        # WARNING: this is used for bytecodes for which it is safe to
        # directly call the primitive.  In general, it is not safe: for
        # example, depending on the type of the receiver, bytecodePrimAt
        # may invoke primitives.AT, primitives.STRING_AT, or anything
        # else that the user put in a class in an 'at:' method.
        # The rule of thumb is that primitives with only int and float
        # in their unwrap_spec are safe.
        try:
            return func(interp, self, argcount)
        except primitives.PrimitiveFailedError:
            pass
        return self._sendSelfSelectorSpecial(selector, argcount, interp)
    callPrimitive.func_name = "callPrimitive_%s" % func.func_name
    return callPrimitive

def make_call_primitive_bytecode_classbased(a_class_name, a_primitive, alternative_class_name, alternative_primitive, selector, argcount):
    @bytecode_implementation()
    def callClassbasedPrimitive(self, interp, current_bytecode):
        rcvr = self.peek(argcount)
        receiver_class = rcvr.getclass(self.space)
        try:
            if receiver_class is getattr(self.space, a_class_name):
                func = primitives.prim_table[a_primitive]
                return func(interp, self, argcount)
            elif receiver_class is getattr(self.space, alternative_class_name):
                func = primitives.prim_table[alternative_primitive]
                return func(interp, self, argcount)
        except primitives.PrimitiveFailedError:
            pass
        return self._sendSelfSelectorSpecial(selector, argcount, interp)
    callClassbasedPrimitive.func_name = "callClassbasedPrimitive_%s" % selector
    return callClassbasedPrimitive

# Some selectors cannot be overwritten, therefore no need to handle PrimitiveFailed.
def make_quick_call_primitive_bytecode(primitive_index, argcount):
    func = primitives.prim_table[primitive_index]
    @bytecode_implementation()
    def quick_call_primitive_bytecode(self, interp, current_bytecode):
        return func(interp, self, argcount)
    return quick_call_primitive_bytecode

# This is for bytecodes that actually implement a simple message-send.
# We do not optimize anything for these cases.
def make_send_selector_bytecode(selector, argcount):
    @bytecode_implementation()
    def selector_bytecode(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial(selector, argcount, interp)
    selector_bytecode.func_name = "selector_bytecode_%s" % selector
    return selector_bytecode

# ___________________________________________________________________________
# Bytecode Implementations:
#
# "self" is always a ContextPartShadow instance.

# __extend__ adds new methods to the ContextPartShadow class
class __extend__(ContextPartShadow):

    # ====== Push/Pop bytecodes ======

    @bytecode_implementation()
    def pushReceiverVariableBytecode(self, interp, current_bytecode):
        index = current_bytecode & 15
        self.push(self.w_receiver().fetch(self.space, index))

    @bytecode_implementation()
    def pushTemporaryVariableBytecode(self, interp, current_bytecode):
        index = current_bytecode & 15
        self.push(self.gettemp(index))

    @bytecode_implementation()
    def pushLiteralConstantBytecode(self, interp, current_bytecode):
        index = current_bytecode & 31
        self.push(self.w_method().getliteral(index))

    @bytecode_implementation()
    def pushLiteralVariableBytecode(self, interp, current_bytecode):
        # this bytecode assumes that literals[index] is an Association
        # which is an object with two named vars, and fetches the second
        # named var (the value).
        index = current_bytecode & 31
        w_association = self.w_method().getliteral(index)
        association = wrapper.AssociationWrapper(self.space, w_association)
        self.push(association.value())

    @bytecode_implementation()
    def storeAndPopReceiverVariableBytecode(self, interp, current_bytecode):
        index = current_bytecode & 7
        self.w_receiver().store(self.space, index, self.pop())

    @bytecode_implementation()
    def storeAndPopTemporaryVariableBytecode(self, interp, current_bytecode):
        index = current_bytecode & 7
        self.settemp(index, self.pop())

    @bytecode_implementation()
    def pushReceiverBytecode(self, interp, current_bytecode):
        self.push(self.w_receiver())

    @bytecode_implementation()
    def pushConstantTrueBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_true)

    @bytecode_implementation()
    def pushConstantFalseBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_false)

    @bytecode_implementation()
    def pushConstantNilBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_nil)

    @bytecode_implementation()
    def pushConstantMinusOneBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_minus_one)

    @bytecode_implementation()
    def pushConstantZeroBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_zero)

    @bytecode_implementation()
    def pushConstantOneBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_one)

    @bytecode_implementation()
    def pushConstantTwoBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_two)

    @bytecode_implementation()
    def pushActiveContextBytecode(self, interp, current_bytecode):
        self.push(self.w_self())

    @bytecode_implementation()
    def duplicateTopBytecode(self, interp, current_bytecode):
        self.push(self.top())

    @bytecode_implementation()
    def popStackBytecode(self, interp, current_bytecode):
        self.pop()

    @bytecode_implementation(parameter_bytes=1)
    def pushNewArrayBytecode(self, interp, current_bytecode, descriptor):
        arraySize, popIntoArray = splitter[7, 1](descriptor)
        newArray = None
        if popIntoArray == 1:
           newArray = interp.space.wrap_list(self.pop_and_return_n(arraySize))
        else:
           newArray = interp.space.w_Array.as_class_get_shadow(interp.space).new(arraySize)
        self.push(newArray)

    # ====== Extended Push/Pop bytecodes ======

    def _extendedVariableTypeAndIndex(self, descriptor):
        return ((descriptor >> 6) & 3), (descriptor & 63)

    @bytecode_implementation(parameter_bytes=1)
    def extendedPushBytecode(self, interp, current_bytecode, descriptor):
        variableType, variableIndex = self._extendedVariableTypeAndIndex(descriptor)
        if variableType == 0:
            self.push(self.w_receiver().fetch(self.space, variableIndex))
        elif variableType == 1:
            self.push(self.gettemp(variableIndex))
        elif variableType == 2:
            self.push(self.w_method().getliteral(variableIndex))
        elif variableType == 3:
            w_association = self.w_method().getliteral(variableIndex)
            association = wrapper.AssociationWrapper(self.space, w_association)
            self.push(association.value())
        else:
            assert 0

    def _extendedStoreBytecode(self, interp, current_bytecode, descriptor):
        variableType, variableIndex = self._extendedVariableTypeAndIndex(descriptor)
        if variableType == 0:
            self.w_receiver().store(self.space, variableIndex, self.top())
        elif variableType == 1:
            self.settemp(variableIndex, self.top())
        elif variableType == 2:
            raise IllegalStoreError
        elif variableType == 3:
            w_association = self.w_method().getliteral(variableIndex)
            association = wrapper.AssociationWrapper(self.space, w_association)
            association.store_value(self.top())

    @bytecode_implementation(parameter_bytes=1)
    def extendedStoreBytecode(self, interp, current_bytecode, descriptor):
        return self._extendedStoreBytecode(interp, current_bytecode, descriptor)

    @bytecode_implementation(parameter_bytes=1)
    def extendedStoreAndPopBytecode(self, interp, current_bytecode, descriptor):
        self._extendedStoreBytecode(interp, current_bytecode, descriptor)
        self.pop()

    def _extract_index_and_temps(self, index_in_array, index_of_array):
        w_indirectTemps = self.gettemp(index_of_array)
        return index_in_array, w_indirectTemps

    @bytecode_implementation(parameter_bytes=2)
    def pushRemoteTempLongBytecode(self, interp, current_bytecode, index_in_array, index_of_array):
        index_in_array, w_indirectTemps = self._extract_index_and_temps(index_in_array, index_of_array)
        self.push(w_indirectTemps.at0(self.space, index_in_array))

    @bytecode_implementation(parameter_bytes=2)
    def storeRemoteTempLongBytecode(self, interp, current_bytecode, index_in_array, index_of_array):
        index_in_array, w_indirectTemps = self._extract_index_and_temps(index_in_array, index_of_array)
        w_indirectTemps.atput0(self.space, index_in_array, self.top())

    @bytecode_implementation(parameter_bytes=2)
    def storeAndPopRemoteTempLongBytecode(self, interp, current_bytecode, index_in_array, index_of_array):
        index_in_array, w_indirectTemps = self._extract_index_and_temps(index_in_array, index_of_array)
        w_indirectTemps.atput0(self.space, index_in_array, self.pop())

    @bytecode_implementation(parameter_bytes=3)
    def pushClosureCopyCopiedValuesBytecode(self, interp, current_bytecode, descriptor, j, i):
        """ Copied from Blogpost: http://www.mirandabanda.org/cogblog/2008/07/22/closures-part-ii-the-bytecodes/
        ContextPart>>pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize
        "Simulate the action of a 'closure copy' bytecode whose result is the
         new BlockClosure for the following code"
        | copiedValues |
        numCopied > 0
                 ifTrue:
                          [copiedValues := Array new: numCopied.
                           numCopied to: 1 by: -1 do:
                                   [:i|
                                   copiedValues at: i put: self pop]]
                 ifFalse:
                          [copiedValues := nil].
        self push: (BlockClosure new
                                   outerContext: self
                                   startpc: pc
                                   numArgs: numArgs
                                   copiedValues: copiedValues).
        self jump: blockSize
        """

        space = self.space
        numArgs, numCopied = splitter[4, 4](descriptor)
        blockSize = (j << 8) | i
        # Create new instance of BlockClosure
        w_closure = space.newClosure(self.w_self(), self.pc(), numArgs,
                                            self.pop_and_return_n(numCopied))
        self.push(w_closure)
        self._jump(blockSize)

    # ====== Helpers for send/return bytecodes ======

    def _sendSelfSelector(self, w_selector, argcount, interp):
        receiver = self.peek(argcount)
        return self._sendSelector(w_selector, argcount, interp,
                                  receiver, receiver.class_shadow(self.space))

    def _sendSuperSelector(self, w_selector, argcount, interp):
        compiledin_class = self.w_method().compiled_in()
        assert isinstance(compiledin_class, model.W_PointersObject)
        s_compiledin = compiledin_class.as_class_get_shadow(self.space)
        return self._sendSelector(w_selector, argcount, interp, self.w_receiver(),
                                  s_compiledin.s_superclass())

    def _sendSelector(self, w_selector, argcount, interp,
                      receiver, receiverclassshadow, w_arguments=None):
        assert argcount >= 0
        try:
            w_method = receiverclassshadow.lookup(w_selector)
        except MethodNotFound:
            return self._doesNotUnderstand(w_selector, argcount, interp, receiver)
        
        code = w_method.primitive()
        if code:
            if w_arguments:
                self.push_all(w_arguments)
            try:
                return self._call_primitive(code, interp, argcount, w_method, w_selector)
            except primitives.PrimitiveFailedError:
                pass # ignore this error and fall back to the Smalltalk version
        if not w_arguments:
            w_arguments = self.pop_and_return_n(argcount)
        s_frame = w_method.create_frame(interp.space, receiver, w_arguments)
        self.pop() # receiver

        # ######################################################################
        if interp.is_tracing():
            interp.print_padded('-> ' + s_frame.short_str())

        return interp.stack_frame(s_frame, self)

    @objectmodel.specialize.arg(1)
    def _sendSelfSelectorSpecial(self, selector, numargs, interp):
        w_selector = self.space.get_special_selector(selector)
        return self._sendSelfSelector(w_selector, numargs, interp)

    def _sendSpecialSelector(self, interp, receiver, special_selector, w_args=[]):
        w_special_selector = self.space.objtable["w_" + special_selector]
        s_class = receiver.class_shadow(self.space)
        w_method = s_class.lookup(w_special_selector)
        s_frame = w_method.create_frame(interp.space, receiver, w_args)
        
        # ######################################################################
        if interp.is_tracing():
            interp.print_padded('-> %s %s' % (special_selector, s_frame.short_str()))
            if not objectmodel.we_are_translated():
                import pdb; pdb.set_trace()

        return interp.stack_frame(s_frame, self)

    def _doesNotUnderstand(self, w_selector, argcount, interp, receiver):
        arguments = self.pop_and_return_n(argcount)
        w_message_class = self.space.classtable["w_Message"]
        assert isinstance(w_message_class, model.W_PointersObject)
        s_message_class = w_message_class.as_class_get_shadow(self.space)
        w_message = s_message_class.new()
        w_message.store(self.space, 0, w_selector)
        w_message.store(self.space, 1, self.space.wrap_list(arguments))
        self.pop() # The receiver, already known.

        try:
            if interp.space.headless.is_set():
                primitives.exitFromHeadlessExecution(self, "doesNotUnderstand:", w_message)
            return self._sendSpecialSelector(interp, receiver, "doesNotUnderstand", [w_message])
        except MethodNotFound:
            from spyvm.shadow import ClassShadow
            s_class = receiver.class_shadow(self.space)
            assert isinstance(s_class, ClassShadow)
            from spyvm import error
            raise error.Exit("Missing doesNotUnderstand in hierarchy of %s" % s_class.getname())

    def _mustBeBoolean(self, interp, receiver):
        return self._sendSpecialSelector(interp, receiver, "mustBeBoolean")

    def _call_primitive(self, code, interp, argcount, w_method, w_selector):
        # ##################################################################
        if interp.is_tracing():
            interp.print_padded("-> primitive %d \t(in %s, named #%s)" % (
                                    code, self.w_method().get_identifier_string(), w_selector.str_content()))
        func = primitives.prim_holder.prim_table[code]
        try:
            # note: argcount does not include rcvr
            # the primitive pushes the result (if any) onto the stack itself
            return func(interp, self, argcount, w_method)
        except primitives.PrimitiveFailedError, e:
            if interp.is_tracing():
                interp.print_padded("-- primitive %d FAILED\t (in %s, named %s)" % (
                            code, w_method.safe_identifier_string(), w_selector.str_content()))
            raise e

    def _return(self, return_value, interp, local_return=False):
        # unfortunately, this assert is not true for some tests. TODO fix this.
        # assert self._stack_ptr == self.tempsize()
        
        # ##################################################################
        if interp.is_tracing():
            interp.print_padded('<- ' + return_value.as_repr_string())
        
        if self.home_is_self() or local_return:
            # a local return just needs to go up the stack once. there
            # it will find the sender as a local, and we don't have to
            # force the reference
            s_return_to = None
            return_from_top = self.s_sender() is None
        else:
            s_return_to = self.s_home().s_sender()
            return_from_top = s_return_to is None
        
        if return_from_top:
            # This should never happen while executing a normal image.
            raise ReturnFromTopLevel(return_value)
        else:
            raise Return(s_return_to, return_value)

    # ====== Send/Return bytecodes ======

    @bytecode_implementation()
    def returnReceiverBytecode(self, interp, current_bytecode):
        return self._return(self.w_receiver(), interp)

    @bytecode_implementation()
    def returnTrueBytecode(self, interp, current_bytecode):
        return self._return(interp.space.w_true, interp)

    @bytecode_implementation()
    def returnFalseBytecode(self, interp, current_bytecode):
        return self._return(interp.space.w_false, interp)

    @bytecode_implementation()
    def returnNilBytecode(self, interp, current_bytecode):
        return self._return(interp.space.w_nil, interp)

    @bytecode_implementation()
    def returnTopFromMethodBytecode(self, interp, current_bytecode):
        return self._return(self.pop(), interp)

    @bytecode_implementation()
    def returnTopFromBlockBytecode(self, interp, current_bytecode):
        return self._return(self.pop(), interp, local_return=True)

    @bytecode_implementation()
    def sendLiteralSelectorBytecode(self, interp, current_bytecode):
        w_selector = self.w_method().getliteral(current_bytecode & 15)
        argcount = ((current_bytecode >> 4) & 3) - 1
        return self._sendSelfSelector(w_selector, argcount, interp)

    def _getExtendedSelectorArgcount(self, descriptor):
        return ((self.w_method().getliteral(descriptor & 31)),
                (descriptor >> 5))

    @bytecode_implementation(parameter_bytes=1)
    def singleExtendedSendBytecode(self, interp, current_bytecode, descriptor):
        w_selector, argcount = self._getExtendedSelectorArgcount(descriptor)
        return self._sendSelfSelector(w_selector, argcount, interp)

    @bytecode_implementation(parameter_bytes=2)
    def doubleExtendedDoAnythingBytecode(self, interp, current_bytecode, second, third):
        from spyvm import error
        opType = second >> 5
        if opType == 0:
            # selfsend
            return self._sendSelfSelector(self.w_method().getliteral(third),
                                          second & 31, interp)
        elif opType == 1:
            # supersend
            return self._sendSuperSelector(self.w_method().getliteral(third),
                                           second & 31, interp)
        elif opType == 2:
            # pushReceiver
            self.push(self.w_receiver().fetch(self.space, third))
        elif opType == 3:
            # pushLiteralConstant
            self.push(self.w_method().getliteral(third))
        elif opType == 4:
            # pushLiteralVariable
            w_association = self.w_method().getliteral(third)
            association = wrapper.AssociationWrapper(self.space, w_association)
            self.push(association.value())
        elif opType == 5:
            self.w_receiver().store(self.space, third, self.top())
        elif opType == 6:
            self.w_receiver().store(self.space, third, self.pop())
        elif opType == 7:
            w_association = self.w_method().getliteral(third)
            association = wrapper.AssociationWrapper(self.space, w_association)
            association.store_value(self.top())

    @bytecode_implementation(parameter_bytes=1)
    def singleExtendedSuperBytecode(self, interp, current_bytecode, descriptor):
        w_selector, argcount = self._getExtendedSelectorArgcount(descriptor)
        return self._sendSuperSelector(w_selector, argcount, interp)

    @bytecode_implementation(parameter_bytes=1)
    def secondExtendedSendBytecode(self, interp, current_bytecode, descriptor):
        w_selector = self.w_method().getliteral(descriptor & 63)
        argcount = descriptor >> 6
        return self._sendSelfSelector(w_selector, argcount, interp)

    # ====== Misc ======

    def _activate_unwind_context(self, interp):
        if self.is_closure_context() or not self.is_BlockClosure_ensure():
            self.mark_returned()
            return
        # The first temp is executed flag for both #ensure: and #ifCurtailed:
        if self.gettemp(1).is_nil(self.space):
            self.settemp(1, self.space.w_true) # mark unwound
            self.push(self.gettemp(0)) # push the first argument
            try:
                self.bytecodePrimValue(interp, 0)
            except Return, nlr:
                assert nlr.s_target_context or nlr.is_local
                if self is not nlr.s_target_context and not nlr.is_local:
                    raise nlr
            finally:
                self.mark_returned()

    @bytecode_implementation()
    def unknownBytecode(self, interp, current_bytecode):
        raise MissingBytecode("unknownBytecode")

    @bytecode_implementation()
    def experimentalBytecode(self, interp, current_bytecode):
        raise MissingBytecode("experimentalBytecode")

    # ====== Jump bytecodes ======

    def _jump(self, offset):
        self.store_pc(self.pc() + offset)

    def _jumpConditional(self, interp, expecting_true, position):
        if expecting_true:
            w_expected = interp.space.w_true
            w_alternative = interp.space.w_false
        else:
            w_alternative = interp.space.w_true
            w_expected = interp.space.w_false

        # Don't check the class, just compare with only two Boolean instances.
        w_bool = self.pop()
        if w_expected.is_same_object(w_bool):
            self._jump(position)
        elif not w_alternative.is_same_object(w_bool):
            self._mustBeBoolean(interp, w_bool)

    def _shortJumpOffset(self, current_bytecode):
        return (current_bytecode & 7) + 1

    def _longJumpOffset(self, current_bytecode, parameter):
        return ((current_bytecode & 3) << 8) + parameter

    @bytecode_implementation()
    def shortUnconditionalJumpBytecode(self, interp, current_bytecode):
        self._jump(self._shortJumpOffset(current_bytecode))

    @bytecode_implementation()
    def shortConditionalJumpBytecode(self, interp, current_bytecode):
        # The conditional jump is "jump on false"
        self._jumpConditional(interp, False, self._shortJumpOffset(current_bytecode))

    @bytecode_implementation(parameter_bytes=1)
    def longUnconditionalJumpBytecode(self, interp, current_bytecode, parameter):
        offset = (((current_bytecode & 7) - 4) << 8) + parameter
        self._jump(offset)

    @bytecode_implementation(parameter_bytes=1)
    def longJumpIfTrueBytecode(self, interp, current_bytecode, parameter):
        self._jumpConditional(interp, True, self._longJumpOffset(current_bytecode, parameter))

    @bytecode_implementation(parameter_bytes=1)
    def longJumpIfFalseBytecode(self, interp, current_bytecode, parameter):
        self._jumpConditional(interp, False, self._longJumpOffset(current_bytecode, parameter))

    # ====== Bytecodes implemented with primitives and message sends ======

    bytecodePrimAdd = make_call_primitive_bytecode(primitives.ADD, "+", 1)
    bytecodePrimSubtract = make_call_primitive_bytecode(primitives.SUBTRACT, "-", 1)
    bytecodePrimLessThan = make_call_primitive_bytecode (primitives.LESSTHAN, "<", 1)
    bytecodePrimGreaterThan = make_call_primitive_bytecode(primitives.GREATERTHAN, ">", 1)
    bytecodePrimLessOrEqual = make_call_primitive_bytecode(primitives.LESSOREQUAL,  "<=", 1)
    bytecodePrimGreaterOrEqual = make_call_primitive_bytecode(primitives.GREATEROREQUAL,  ">=", 1)
    bytecodePrimEqual = make_call_primitive_bytecode(primitives.EQUAL,   "=", 1)
    bytecodePrimNotEqual = make_call_primitive_bytecode(primitives.NOTEQUAL,  "~=", 1)
    bytecodePrimMultiply = make_call_primitive_bytecode(primitives.MULTIPLY,  "*", 1)
    bytecodePrimDivide = make_call_primitive_bytecode(primitives.DIVIDE,  "/", 1)
    bytecodePrimMod = make_call_primitive_bytecode(primitives.MOD, "\\\\", 1)
    bytecodePrimMakePoint = make_call_primitive_bytecode(primitives.MAKE_POINT, "@", 1)
    bytecodePrimBitShift = make_call_primitive_bytecode(primitives.BIT_SHIFT, "bitShift:", 1)
    bytecodePrimDiv = make_call_primitive_bytecode(primitives.DIV, "//", 1)
    bytecodePrimBitAnd = make_call_primitive_bytecode(primitives.BIT_AND, "bitAnd:", 1)
    bytecodePrimBitOr = make_call_primitive_bytecode(primitives.BIT_OR, "bitOr:", 1)

    bytecodePrimAt = make_send_selector_bytecode("at:", 1)
    bytecodePrimAtPut = make_send_selector_bytecode("at:put:", 2)
    bytecodePrimSize = make_send_selector_bytecode("size", 0)
    bytecodePrimNext = make_send_selector_bytecode("next", 0)
    bytecodePrimNextPut = make_send_selector_bytecode("nextPut:", 1)
    bytecodePrimAtEnd = make_send_selector_bytecode("atEnd", 0)

    bytecodePrimEquivalent = make_quick_call_primitive_bytecode(primitives.EQUIVALENT, 1)
    bytecodePrimClass = make_quick_call_primitive_bytecode(primitives.CLASS, 0)

    bytecodePrimBlockCopy = make_call_primitive_bytecode(primitives.BLOCK_COPY, "blockCopy:", 1)
    bytecodePrimValue = make_call_primitive_bytecode_classbased("w_BlockContext", primitives.VALUE, "w_BlockClosure", primitives.CLOSURE_VALUE, "value", 0)
    bytecodePrimValueWithArg = make_call_primitive_bytecode_classbased("w_BlockContext", primitives.VALUE, "w_BlockClosure", primitives.CLOSURE_VALUE_, "value:", 1)

    bytecodePrimDo = make_send_selector_bytecode("do:", 1)
    bytecodePrimNew = make_send_selector_bytecode("new", 0)
    bytecodePrimNewWithArg = make_send_selector_bytecode("new:", 1)
    bytecodePrimPointX = make_send_selector_bytecode("x", 0)
    bytecodePrimPointY = make_send_selector_bytecode("y", 0)

BYTECODE_RANGES = [
            (  0,  15, "pushReceiverVariableBytecode"),
            ( 16,  31, "pushTemporaryVariableBytecode"),
            ( 32,  63, "pushLiteralConstantBytecode"),
            ( 64,  95, "pushLiteralVariableBytecode"),
            ( 96, 103, "storeAndPopReceiverVariableBytecode"),
            (104, 111, "storeAndPopTemporaryVariableBytecode"),
            (112, "pushReceiverBytecode"),
            (113, "pushConstantTrueBytecode"),
            (114, "pushConstantFalseBytecode"),
            (115, "pushConstantNilBytecode"),
            (116, "pushConstantMinusOneBytecode"),
            (117, "pushConstantZeroBytecode"),
            (118, "pushConstantOneBytecode"),
            (119, "pushConstantTwoBytecode"),
            (120, "returnReceiverBytecode"),
            (121, "returnTrueBytecode"),
            (122, "returnFalseBytecode"),
            (123, "returnNilBytecode"),
            (124, "returnTopFromMethodBytecode"),
            (125, "returnTopFromBlockBytecode"),
            (126, "unknownBytecode"),
            (127, "unknownBytecode"),
            (128, "extendedPushBytecode"),
            (129, "extendedStoreBytecode"),
            (130, "extendedStoreAndPopBytecode"),
            (131, "singleExtendedSendBytecode"),
            (132, "doubleExtendedDoAnythingBytecode"),
            (133, "singleExtendedSuperBytecode"),
            (134, "secondExtendedSendBytecode"),
            (135, "popStackBytecode"),
            (136, "duplicateTopBytecode"),
            (137, "pushActiveContextBytecode"),
            (138, "pushNewArrayBytecode"),
            (139, "experimentalBytecode"),
            (140, "pushRemoteTempLongBytecode"),
            (141, "storeRemoteTempLongBytecode"),
            (142, "storeAndPopRemoteTempLongBytecode"),
            (143, "pushClosureCopyCopiedValuesBytecode"),
            (144, 151, "shortUnconditionalJumpBytecode"),
            (152, 159, "shortConditionalJumpBytecode"),
            (160, 167, "longUnconditionalJumpBytecode"),
            (168, 171, "longJumpIfTrueBytecode"),
            (172, 175, "longJumpIfFalseBytecode"),
            (176, "bytecodePrimAdd"),
            (177, "bytecodePrimSubtract"),
            (178, "bytecodePrimLessThan"),
            (179, "bytecodePrimGreaterThan"),
            (180, "bytecodePrimLessOrEqual"),
            (181, "bytecodePrimGreaterOrEqual"),
            (182, "bytecodePrimEqual"),
            (183, "bytecodePrimNotEqual"),
            (184, "bytecodePrimMultiply"),
            (185, "bytecodePrimDivide"),
            (186, "bytecodePrimMod"),
            (187, "bytecodePrimMakePoint"),
            (188, "bytecodePrimBitShift"),
            (189, "bytecodePrimDiv"),
            (190, "bytecodePrimBitAnd"),
            (191, "bytecodePrimBitOr"),
            (192, "bytecodePrimAt"),
            (193, "bytecodePrimAtPut"),
            (194, "bytecodePrimSize"),
            (195, "bytecodePrimNext"),
            (196, "bytecodePrimNextPut"),
            (197, "bytecodePrimAtEnd"),
            (198, "bytecodePrimEquivalent"),
            (199, "bytecodePrimClass"),
            (200, "bytecodePrimBlockCopy"),
            (201, "bytecodePrimValue"),
            (202, "bytecodePrimValueWithArg"),
            (203, "bytecodePrimDo"),
            (204, "bytecodePrimNew"),
            (205, "bytecodePrimNewWithArg"),
            (206, "bytecodePrimPointX"),
            (207, "bytecodePrimPointY"),
            (208, 255, "sendLiteralSelectorBytecode"),
            ]

from rpython.rlib.unroll import unrolling_iterable
UNROLLING_BYTECODE_RANGES = unrolling_iterable(BYTECODE_RANGES)

def initialize_bytecode_names():
    result = [None] * 256
    for entry in BYTECODE_RANGES:
        if len(entry) == 2:
            result[entry[0]] = entry[1]
        else:
            for arg, pos in enumerate(range(entry[0], entry[1]+1)):
                result[pos] = "%s(%s)" % (entry[2], arg)
    assert None not in result
    return result

BYTECODE_NAMES = initialize_bytecode_names()

def initialize_bytecode_table():
    result = [None] * 256
    for entry in BYTECODE_RANGES:
        if len(entry) == 2:
            positions = [entry[0]]
        else:
            positions = range(entry[0], entry[1]+1)
        for pos in positions:
            result[pos] = getattr(ContextPartShadow, entry[-1])
    assert None not in result
    return result

# this table is only used for creating named bytecodes in tests and printing
BYTECODE_TABLE = initialize_bytecode_table()

# Smalltalk debugging facilities, patching Interpreter and ContextPartShadow
# in order to enable tracing/jumping for message sends etc.
def debugging():
    def stepping_debugger_init(original):
        def meth(self, space, image=None, image_name="", trace=False):
            return_value = original(self, space, image=image,
                                    image_name=image_name, trace=trace)
            # ##############################################################

            self.message_stepping = False
            self.halt_on_failing_primitives = False

            # ##############################################################
            return return_value
        return meth

    Interpreter.__init__ = stepping_debugger_init(Interpreter.__init__)

    def stepping_debugger_send(original):
        """When interp.message_stepping is True, we halt on every call of ContextPartShadow._sendSelector.
        The method is not called for bytecode message sends (see constants.SPECIAL_SELECTORS)"""
        def meth(s_context, w_selector, argcount, interp,
                      receiver, receiverclassshadow):
            options = [False]
            def next(): interp.message_stepping = True; print 'Now continue (c).'
            def over(): options[0] = True; print  'Skipping #%s. You still need to continue(c).' % w_selector.str_content()
            def pstack(): print s_context.print_stack()
            if interp.message_stepping:
                if argcount == 0:
                    print "-> %s #%s" % (receiver.as_repr_string(),
                            w_selector.str_content())
                elif argcount == 1:
                    print "-> %s #%s %s" % (receiver.as_repr_string(),
                            w_selector.str_content(),
                            s_context.peek(0).as_repr_string())
                else:
                    print "-> %s #%s %r" % (receiver.as_repr_string(),
                            w_selector.str_content(),
                            [s_context.peek(argcount-1-i) for i in range(argcount)])
                import pdb; pdb.set_trace()
            if options[0]:
                m_s = interp.message_stepping
                interp.message_stepping = False
                try:
                    return original(s_context, w_selector, argcount, interp, receiver, receiverclassshadow)
                finally:
                    interp.message_stepping = m_s
            else:
                return original(s_context, w_selector, argcount, interp, receiver, receiverclassshadow)
        return meth

    ContextPartShadow._sendSelector = stepping_debugger_send(ContextPartShadow._sendSelector)

    def stepping_debugger_failed_primitive_halt(original):
        def meth(self, code, interp, argcount, w_method, w_selector):
            try:
                original(self, code, interp, argcount, w_method, w_selector)
            except primitives.PrimitiveFailedError, e:
                if interp.halt_on_failing_primitives:
                    func = primitives.prim_holder.prim_table[code]
                    if func.func_name != 'raise_failing_default' and code != 83:
                        import pdb; pdb.set_trace()
                        try:
                            func(interp, self, argcount, w_method) # will fail again
                        except primitives.PrimitiveFailedError:
                            pass
                raise e
        return meth

    ContextPartShadow._call_primitive = stepping_debugger_failed_primitive_halt(ContextPartShadow._call_primitive)

    def trace_missing_named_primitives(original):
        def meth(interp, s_frame, argcount, w_method=None):
            try:
                return original(interp, s_frame, argcount, w_method=w_method)
            except primitives.PrimitiveFailedError, e:
                space = interp.space
                w_description = w_method.literalat0(space, 1)
                if not isinstance(w_description, model.W_PointersObject) or w_description.size() < 2:
                    raise e
                w_modulename = w_description.at0(space, 0)
                w_functionname = w_description.at0(space, 1)
                if not (isinstance(w_modulename, model.W_BytesObject) and
                        isinstance(w_functionname, model.W_BytesObject)):
                    raise e
                signature = (w_modulename.as_string(), w_functionname.as_string())
                debugging.missing_named_primitives.add(signature)
                raise e
        return meth

    primitives.prim_table[primitives.EXTERNAL_CALL] = trace_missing_named_primitives(primitives.prim_table[primitives.EXTERNAL_CALL])
    debugging.missing_named_primitives = set()

# debugging()
