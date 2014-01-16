import py
import os
from spyvm.shadow import ContextPartShadow, MethodContextShadow, BlockContextShadow, MethodNotFound
from spyvm import model, constants, primitives, conftest, wrapper
from spyvm.tool.bitmanipulation import splitter

from rpython.rlib import jit
from rpython.rlib import objectmodel, unroll

class MissingBytecode(Exception):
    """Bytecode not implemented yet."""
    def __init__(self, bytecodename):
        self.bytecodename = bytecodename
        print "MissingBytecode:", bytecodename     # hack for debugging

class IllegalStoreError(Exception):
    """Illegal Store."""

def get_printable_location(pc, self, method):
    bc = ord(method.bytecode[pc])
    name = method._w_self._likely_methodname
    return '%d: [%s]%s (%s)' % (pc, hex(bc), BYTECODE_NAMES[bc], name)


class Interpreter(object):
    _immutable_fields_ = ["space", "image", "image_name",
                          "max_stack_depth", "interrupt_counter_size",
                          "startup_time", "evented"]
    _w_last_active_context = None
    cnt = 0
    _last_indent = ""
    jit_driver = jit.JitDriver(
        greens=['pc', 'self', 'method'],
        reds=['s_context'],
        virtualizables=['s_context'],
        get_printable_location=get_printable_location
    )

    def __init__(self, space, image=None, image_name="", trace=False,
                 evented=True,
                 max_stack_depth=constants.MAX_LOOP_DEPTH):
        import time
        self.space = space
        self.image = image
        self.image_name = image_name
        if image:
            self.startup_time = image.startup_time
        else:
            self.startup_time = constants.CompileTime
        self.max_stack_depth = max_stack_depth
        self.remaining_stack_depth = max_stack_depth
        self._loop = False
        self.next_wakeup_tick = 0
        self.evented = evented
        try:
            self.interrupt_counter_size = int(os.environ["SPY_ICS"])
        except KeyError:
            self.interrupt_counter_size = constants.INTERRUPT_COUNTER_SIZE
        self.interrupt_check_counter = self.interrupt_counter_size
        # ######################################################################
        self.trace = trace
        self.trace_proxy = False

    def interpret_with_w_frame(self, w_frame):
        try:
            self.loop(w_frame)
        except ReturnFromTopLevel, e:
            return e.object

    def should_trace(self, primitives=False):
        if objectmodel.we_are_translated() or conftest.option is None:
            return False
        if not primitives:
            return conftest.option.bc_trace
        return conftest.option.prim_trace

    def loop(self, w_active_context):
        # just a trampoline for the actual loop implemented in c_loop
        self._loop = True
        s_new_context = w_active_context.as_context_get_shadow(self.space)
        while True:
            assert self.remaining_stack_depth == self.max_stack_depth
            # Need to save s_sender, c_loop will nil this on return
            s_sender = s_new_context.s_sender()
            try:
                s_new_context = self.c_loop(s_new_context)
            except StackOverflow, e:
                s_new_context = e.s_context
            except Return, nlr:
                s_new_context = s_sender
                while s_new_context is not nlr.s_target_context:
                    s_sender = s_new_context.s_sender()
                    if not s_new_context.is_closure_context() and s_new_context.s_method().primitive() == 198:
                        s_new_context.activate_unwind_context(self)
                    s_new_context.mark_returned()
                    s_new_context = s_sender
                s_new_context.push(nlr.value)
            except ProcessSwitch, p:
                if self.trace:
                    print "====== Switch from: %s to: %s ======" % (s_new_context.short_str(), p.s_new_context.short_str())
                s_new_context = p.s_new_context

    def c_loop(self, s_context, may_context_switch=True):
        old_pc = 0
        if not jit.we_are_jitted() and may_context_switch:
            self.quick_check_for_interrupt(s_context)
        method = s_context.s_method()
        while True:
            pc = s_context.pc()
            if pc < old_pc:
                if jit.we_are_jitted():
                    self.quick_check_for_interrupt(s_context,
                                    dec=self._get_adapted_tick_counter())
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
                if nlr.s_target_context is not s_context:
                    if not s_context.is_closure_context() and s_context.s_method().primitive() == 198:
                        s_context.activate_unwind_context(self)
                    s_context.mark_returned()
                    raise nlr
                else:
                    s_context.push(nlr.value)

    def _get_adapted_tick_counter(self):
        # Normally, the tick counter is decremented by 1 for every message send.
        # Since we don't know how many messages are called during this trace, we
        # just decrement by 100th of the trace length (num of bytecodes).
        trace_length = jit.current_trace_length()
        decr_by = int(trace_length // 100)
        return max(decr_by, 1)

    def stack_frame(self, s_new_frame, may_context_switch=True):
        if not self._loop:
            return s_new_frame # this test is done to not loop in test,
                               # but rather step just once where wanted
        if self.remaining_stack_depth <= 1:
            raise StackOverflow(s_new_frame)

        self.remaining_stack_depth -= 1
        try:
            retval = self.c_loop(s_new_frame, may_context_switch)
        finally:
            self.remaining_stack_depth += 1
        return retval

    def perform(self, w_receiver, selector, *arguments_w):
        if isinstance(selector, str):
            if selector == "asSymbol":
                w_selector = self.image.w_asSymbol
            else:
                w_selector = self.perform(self.space.wrap_string(selector),
                                            "asSymbol")
        else:
            w_selector = selector

        w_method = model.W_CompiledMethod(header=512)
        w_method.literalatput0(self.space, 1, w_selector)
        assert len(arguments_w) <= 7
        w_method.setbytes([chr(131), chr(len(arguments_w) << 5 + 0), chr(124)]) #returnTopFromMethod
        s_method = w_method.as_compiledmethod_get_shadow(self.space)
        s_frame = MethodContextShadow.make_context(
                self.space, s_method, w_receiver, [], None)
        s_frame.push(w_receiver)
        s_frame.push_all(list(arguments_w))

        self.interrupt_check_counter = self.interrupt_counter_size
        try:
            self.loop(s_frame.w_self())
        except ReturnFromTopLevel, e:
            return e.object

    def quick_check_for_interrupt(self, s_frame, dec=1):
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
            if not semaphore.is_same_object(self.space.w_nil):
                wrapper.SemaphoreWrapper(self.space, semaphore).signal(s_frame.w_self())
        # We have no finalization process, so far.
        # We do not support external semaphores.
            # In cog, the method to add such a semaphore is only called in GC.

    def time_now(self):
        import time
        from rpython.rlib.rarithmetic import intmask
        return intmask((int(time.time() * 1000) - self.startup_time) & constants.TAGGED_MASK)

    def padding(self, symbol=' '):
        return symbol * (self.max_stack_depth - self.remaining_stack_depth)

class ReturnFromTopLevel(Exception):
    _attrs_ = ["object"]
    def __init__(self, object):
        self.object = object

class StackOverflow(Exception):
    _attrs_ = ["s_context"]
    def __init__(self, s_top_context):
        self.s_context = s_top_context

class Return(Exception):
    _attrs_ = ["value", "s_target_context"]
    def __init__(self, object, s_context):
        self.value = object
        self.s_target_context = s_context

class ProcessSwitch(Exception):
    _attrs_ = ["s_new_context"]
    def __init__(self, s_context):
        self.s_new_context = s_context


def make_call_primitive_bytecode(primitive, selector, argcount):
    def callPrimitive(self, interp, current_bytecode):
        # WARNING: this is used for bytecodes for which it is safe to
        # directly call the primitive.  In general, it is not safe: for
        # example, depending on the type of the receiver, bytecodePrimAt
        # may invoke primitives.AT, primitives.STRING_AT, or anything
        # else that the user put in a class in an 'at:' method.
        # The rule of thumb is that primitives with only int and float
        # in their unwrap_spec are safe.
        # XXX move next line out of callPrimitive?
        func = primitives.prim_table[primitive]
        try:
            return func(interp, self, argcount)
        except primitives.PrimitiveFailedError:
            pass
        return self._sendSelfSelectorSpecial(selector, argcount, interp)
    return callPrimitive

def make_call_primitive_bytecode_classbased(a_class_name, a_primitive, alternative_class_name, alternative_primitive, selector, argcount):
    def callPrimitive(self, interp, current_bytecode):
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
    return callPrimitive

# ___________________________________________________________________________
# Bytecode Implementations:
#
# "self" is always a ContextPartShadow instance.

# __extend__ adds new methods to the ContextPartShadow class
class __extend__(ContextPartShadow):
    # push bytecodes
    def pushReceiverVariableBytecode(self, interp, current_bytecode):
        index = current_bytecode & 15
        self.push(self.w_receiver().fetch(self.space, index))

    def pushTemporaryVariableBytecode(self, interp, current_bytecode):
        index = current_bytecode & 15
        self.push(self.gettemp(index))

    def pushLiteralConstantBytecode(self, interp, current_bytecode):
        index = current_bytecode & 31
        self.push(self.s_method().getliteral(index))

    def pushLiteralVariableBytecode(self, interp, current_bytecode):
        # this bytecode assumes that literals[index] is an Association
        # which is an object with two named vars, and fetches the second
        # named var (the value).
        index = current_bytecode & 31
        w_association = self.s_method().getliteral(index)
        association = wrapper.AssociationWrapper(self.space, w_association)
        self.push(association.value())

    def storeAndPopReceiverVariableBytecode(self, interp, current_bytecode):
        index = current_bytecode & 7
        self.w_receiver().store(self.space, index, self.pop())

    def storeAndPopTemporaryVariableBytecode(self, interp, current_bytecode):
        index = current_bytecode & 7
        self.settemp(index, self.pop())

    # push bytecodes
    def pushReceiverBytecode(self, interp, current_bytecode):
        self.push(self.w_receiver())

    def pushConstantTrueBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_true)

    def pushConstantFalseBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_false)

    def pushConstantNilBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_nil)

    def pushConstantMinusOneBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_minus_one)

    def pushConstantZeroBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_zero)

    def pushConstantOneBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_one)

    def pushConstantTwoBytecode(self, interp, current_bytecode):
        self.push(interp.space.w_two)

    def pushActiveContextBytecode(self, interp, current_bytecode):
        self.push(self.w_self())

    def duplicateTopBytecode(self, interp, current_bytecode):
        self.push(self.top())

    # send, return bytecodes
    def sendLiteralSelectorBytecode(self, interp, current_bytecode):
        w_selector = self.s_method().getliteral(current_bytecode & 15)
        argcount = ((current_bytecode >> 4) & 3) - 1
        return self._sendSelfSelector(w_selector, argcount, interp)

    def _sendSelfSelector(self, w_selector, argcount, interp):
        receiver = self.peek(argcount)
        return self._sendSelector(w_selector, argcount, interp,
                                  receiver, receiver.shadow_of_my_class(self.space))

    def _sendSuperSelector(self, w_selector, argcount, interp):
        w_compiledin = self.s_method().w_compiledin
        assert isinstance(w_compiledin, model.W_PointersObject)
        s_compiledin = w_compiledin.as_class_get_shadow(self.space)
        return self._sendSelector(w_selector, argcount, interp, self.w_receiver(),
                                  s_compiledin.s_superclass())

    def _sendSelector(self, w_selector, argcount, interp,
                      receiver, receiverclassshadow):
        assert isinstance(w_selector, model.W_BytesObject)
        if interp.should_trace():
            print "%sSending selector %r to %r with: %r" % (
                interp._last_indent, w_selector.as_repr_string(), receiver,
                [self.peek(argcount-1-i) for i in range(argcount)])
        assert argcount >= 0

        try:
            s_method = receiverclassshadow.lookup(w_selector)
        except MethodNotFound:
            return self._doesNotUnderstand(w_selector, argcount, interp, receiver)

        code = s_method.primitive()
        if code:
            try:
                return self._call_primitive(code, interp, argcount, s_method, w_selector)
            except primitives.PrimitiveFailedError:
                pass # ignore this error and fall back to the Smalltalk version
        arguments = self.pop_and_return_n(argcount)
        s_frame = s_method.create_frame(self.space, receiver, arguments, self)
        self.pop() # receiver

        # ######################################################################
        if interp.trace:
            print interp.padding() + s_frame.short_str()

        return interp.stack_frame(s_frame)

    def _doesNotUnderstand(self, w_selector, argcount, interp, receiver):
        arguments = self.pop_and_return_n(argcount)
        s_message_class = self.space.classtable["w_Message"].as_class_get_shadow(self.space)
        w_message = s_message_class.new()
        w_message.store(self.space, 0, w_selector)
        w_message.store(self.space, 1, self.space.wrap_list(arguments))
        s_class = receiver.shadow_of_my_class(self.space)
        try:
            s_method = s_class.lookup(self.space.objtable["w_doesNotUnderstand"])
        except MethodNotFound:
            from spyvm.shadow import ClassShadow
            assert isinstance(s_class, ClassShadow)
            print "Missing doesDoesNotUnderstand in hierarchy of %s" % s_class.getname()
            raise
        s_frame = s_method.create_frame(self.space, receiver, [w_message], self)
        self.pop()

        # ######################################################################
        if interp.trace:
            print '%s%s missing: #%s' % (interp.padding('#'), s_frame.short_str(), w_selector.as_repr_string())
            if not objectmodel.we_are_translated():
                import pdb; pdb.set_trace()

        return interp.stack_frame(s_frame)

    def _call_primitive(self, code, interp, argcount, s_method, w_selector):
        # the primitive pushes the result (if any) onto the stack itself
        if interp.should_trace():
            print "%sActually calling primitive %d" % (interp._last_indent, code,)
        func = primitives.prim_holder.prim_table[code]
        # ##################################################################
        if interp.trace:
            print "%s-> primitive %d \t(in #%s, named #%s)" % (
                ' ' * (interp.max_stack_depth - interp.remaining_stack_depth),
                    code, self.w_method()._likely_methodname, w_selector.as_repr_string())
        try:
            # note: argcount does not include rcvr
            return func(interp, self, argcount, s_method)
        except primitives.PrimitiveFailedError, e:
            if interp.trace:
                print "%s primitive FAILED" % (
                ' ' * (interp.max_stack_depth - interp.remaining_stack_depth),)

            if interp.should_trace(True):
                print "PRIMITIVE FAILED: %d %s" % (s_method.primitive, w_selector.as_repr_string())
            raise e


    def _return(self, return_value, interp, s_return_to):
        # for tests, when returning from the top-level context
        if s_return_to is None:
            raise ReturnFromTopLevel(return_value)
        # unfortunately, the assert below is not true for some tests
        # assert self._stack_ptr == self.tempsize()

        if interp.trace:
            print '%s<- %s' % (interp.padding(), return_value.as_repr_string())
        raise Return(return_value, s_return_to)

    def activate_unwind_context(self, interp):
        # the first temp is executed flag for both #ensure: and #ifCurtailed:
        if self.gettemp(1) is self.space.w_nil:
            self.settemp(1, self.space.w_true) # mark unwound
            self.push(self.gettemp(0)) # push the first argument
            try:
                self.bytecodePrimValue(interp, 0)
            except Return, nlr:
                if self is nlr.s_target_context:
                    return
                else:
                    self.mark_returned()
                    raise nlr

    def returnReceiver(self, interp, current_bytecode):
        return self._return(self.w_receiver(), interp, self.s_home().s_sender())

    def returnTrue(self, interp, current_bytecode):
        return self._return(interp.space.w_true, interp, self.s_home().s_sender())

    def returnFalse(self, interp, current_bytecode):
        return self._return(interp.space.w_false, interp, self.s_home().s_sender())

    def returnNil(self, interp, current_bytecode):
        return self._return(interp.space.w_nil, interp, self.s_home().s_sender())

    def returnTopFromMethod(self, interp, current_bytecode):
        return self._return(self.pop(), interp, self.s_home().s_sender())

    def returnTopFromBlock(self, interp, current_bytecode):
        return self._return(self.pop(), interp, self.s_sender())

    def unknownBytecode(self, interp, current_bytecode):
        raise MissingBytecode("unknownBytecode")

    def extendedVariableTypeAndIndex(self):
        # AK please explain this method (a helper, I guess)
        descriptor = self.getbytecode()
        return ((descriptor >> 6) & 3), (descriptor & 63)

    def extendedPushBytecode(self, interp, current_bytecode):
        variableType, variableIndex = self.extendedVariableTypeAndIndex()
        if variableType == 0:
            self.push(self.w_receiver().fetch(self.space, variableIndex))
        elif variableType == 1:
            self.push(self.gettemp(variableIndex))
        elif variableType == 2:
            self.push(self.s_method().getliteral(variableIndex))
        elif variableType == 3:
            w_association = self.s_method().getliteral(variableIndex)
            association = wrapper.AssociationWrapper(self.space, w_association)
            self.push(association.value())
        else:
            assert 0

    def extendedStoreBytecode(self, interp, current_bytecode):
        variableType, variableIndex = self.extendedVariableTypeAndIndex()
        if variableType == 0:
            self.w_receiver().store(self.space, variableIndex, self.top())
        elif variableType == 1:
            self.settemp(variableIndex, self.top())
        elif variableType == 2:
            raise IllegalStoreError
        elif variableType == 3:
            w_association = self.s_method().getliteral(variableIndex)
            association = wrapper.AssociationWrapper(self.space, w_association)
            association.store_value(self.top())

    def extendedStoreAndPopBytecode(self, interp, current_bytecode):
        self.extendedStoreBytecode(interp, current_bytecode)
        self.pop()

    def getExtendedSelectorArgcount(self):
        descriptor = self.getbytecode()
        return ((self.s_method().getliteral(descriptor & 31)),
                (descriptor >> 5))

    def singleExtendedSendBytecode(self, interp, current_bytecode):
        w_selector, argcount = self.getExtendedSelectorArgcount()
        return self._sendSelfSelector(w_selector, argcount, interp)

    def doubleExtendedDoAnythingBytecode(self, interp, current_bytecode):
        from spyvm import error
        second = self.getbytecode()
        third = self.getbytecode()
        opType = second >> 5
        if opType == 0:
            # selfsend
            return self._sendSelfSelector(self.s_method().getliteral(third),
                                          second & 31, interp)
        elif opType == 1:
            # supersend
            return self._sendSuperSelector(self.s_method().getliteral(third),
                                           second & 31, interp)
        elif opType == 2:
            # pushReceiver
            self.push(self.w_receiver().fetch(self.space, third))
        elif opType == 3:
            # pushLiteralConstant
            self.push(self.s_method().getliteral(third))
        elif opType == 4:
            # pushLiteralVariable
            w_association = self.s_method().getliteral(third)
            association = wrapper.AssociationWrapper(self.space, w_association)
            self.push(association.value())
        elif opType == 5:
            try:
                self.w_receiver().store(self.space, third, self.top())
            except error.SenderChainManipulation, e:
                raise StackOverflow(self)
        elif opType == 6:
            try:
                self.w_receiver().store(self.space, third, self.pop())
            except error.SenderChainManipulation, e:
                raise StackOverflow(self)
        elif opType == 7:
            w_association = self.s_method().getliteral(third)
            association = wrapper.AssociationWrapper(self.space, w_association)
            association.store_value(self.top())

    def singleExtendedSuperBytecode(self, interp, current_bytecode):
        w_selector, argcount = self.getExtendedSelectorArgcount()
        return self._sendSuperSelector(w_selector, argcount, interp)

    def secondExtendedSendBytecode(self, interp, current_bytecode):
        descriptor = self.getbytecode()
        w_selector = self.s_method().getliteral(descriptor & 63)
        argcount = descriptor >> 6
        return self._sendSelfSelector(w_selector, argcount, interp)

    def popStackBytecode(self, interp, current_bytecode):
        self.pop()

    # closure bytecodes
    def pushNewArrayBytecode(self, interp, current_bytecode):
        arraySize, popIntoArray = splitter[7, 1](self.getbytecode())
        newArray = None
        if popIntoArray == 1:
           newArray = interp.space.wrap_list(self.pop_and_return_n(arraySize))
        else:
           newArray = interp.space.w_Array.as_class_get_shadow(interp.space).new(arraySize)
        self.push(newArray)

    def experimentalBytecode(self, interp, current_bytecode):
        raise MissingBytecode("experimentalBytecode")

    def _extract_index_and_temps(self):
        index_in_array = self.getbytecode()
        index_of_array = self.getbytecode()
        w_indirectTemps = self.gettemp(index_of_array)
        return index_in_array, w_indirectTemps

    def pushRemoteTempLongBytecode(self, interp, current_bytecode):
        index_in_array, w_indirectTemps = self._extract_index_and_temps()
        self.push(w_indirectTemps.at0(self.space, index_in_array))

    def storeRemoteTempLongBytecode(self, interp, current_bytecode):
        index_in_array, w_indirectTemps = self._extract_index_and_temps()
        w_indirectTemps.atput0(self.space, index_in_array, self.top())

    def storeAndPopRemoteTempLongBytecode(self, interp, current_bytecode):
        index_in_array, w_indirectTemps = self._extract_index_and_temps()
        w_indirectTemps.atput0(self.space, index_in_array, self.pop())

    def pushClosureCopyCopiedValuesBytecode(self, interp, current_bytecode):
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
        numArgs, numCopied = splitter[4, 4](self.getbytecode())
        j = self.getbytecode()
        i = self.getbytecode()
        blockSize = (j << 8) | i
        #create new instance of BlockClosure
        w_closure = space.newClosure(self.w_self(), self.pc(), numArgs,
                                            self.pop_and_return_n(numCopied))
        self.push(w_closure)
        self.jump(blockSize)

    def jump(self, offset):
        self.store_pc(self.pc() + offset)

    def jumpConditional(self,bool,position):
        if self.top() == bool: # XXX this seems wrong?
            self.jump(position)
        self.pop()

    def shortJumpPosition(self, current_bytecode):
        return (current_bytecode & 7) + 1

    def shortUnconditionalJump(self, interp, current_bytecode):
        self.jump(self.shortJumpPosition(current_bytecode))

    def shortConditionalJump(self, interp, current_bytecode):
        self.jumpConditional(
                interp.space.w_false, self.shortJumpPosition(current_bytecode))

    def longUnconditionalJump(self, interp, current_bytecode):
        self.jump((((current_bytecode & 7) - 4) << 8) + self.getbytecode())

    def longJumpPosition(self, current_bytecode):
        return ((current_bytecode & 3) << 8) + self.getbytecode()

    def longJumpIfTrue(self, interp, current_bytecode):
        self.jumpConditional(interp.space.w_true, self.longJumpPosition(current_bytecode))

    def longJumpIfFalse(self, interp, current_bytecode):
        self.jumpConditional(interp.space.w_false, self.longJumpPosition(current_bytecode))


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

    @objectmodel.specialize.arg(1)
    def _sendSelfSelectorSpecial(self, selector, numargs, interp):
        w_selector = self.space.get_special_selector(selector)
        return self._sendSelfSelector(w_selector, numargs, interp)

    def bytecodePrimAt(self, interp, current_bytecode):
        # n.b.: depending on the type of the receiver, this may invoke
        # primitives.AT, primitives.STRING_AT, or something else for all
        # I know.
        return self._sendSelfSelectorSpecial("at:", 1, interp)

    def bytecodePrimAtPut(self, interp, current_bytecode):
        # n.b. as above
        return self._sendSelfSelectorSpecial("at:put:", 2, interp)

    def bytecodePrimSize(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial("size", 0, interp)

    def bytecodePrimNext(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial("next", 0, interp)

    def bytecodePrimNextPut(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial("nextPut:", 1, interp)

    def bytecodePrimAtEnd(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial("atEnd", 0, interp)

    def bytecodePrimEquivalent(self, interp, current_bytecode):
        # short-circuit: classes cannot override the '==' method,
        # which cannot fail
        primitives.prim_table[primitives.EQUIVALENT](interp, self, 1)

    def bytecodePrimClass(self, interp, current_bytecode):
        # short-circuit: classes cannot override the 'class' method,
        # which cannot fail
        primitives.prim_table[primitives.CLASS](interp, self, 0)

    bytecodePrimBlockCopy = make_call_primitive_bytecode(primitives.BLOCK_COPY, "blockCopy:", 1)
    bytecodePrimValue = make_call_primitive_bytecode_classbased("w_BlockContext", primitives.VALUE, "w_BlockClosure", primitives.CLOSURE_VALUE, "value", 0)
    bytecodePrimValueWithArg = make_call_primitive_bytecode_classbased("w_BlockContext", primitives.VALUE, "w_BlockClosure", primitives.CLOSURE_VALUE_, "value:", 1)

    def bytecodePrimDo(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial("do:", 1, interp)

    def bytecodePrimNew(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial("new", 0, interp)

    def bytecodePrimNewWithArg(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial("new:", 1, interp)

    def bytecodePrimPointX(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial("x", 0, interp)

    def bytecodePrimPointY(self, interp, current_bytecode):
        return self._sendSelfSelectorSpecial("y", 0, interp)

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
            (120, "returnReceiver"),
            (121, "returnTrue"),
            (122, "returnFalse"),
            (123, "returnNil"),
            (124, "returnTopFromMethod"),
            (125, "returnTopFromBlock"),
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
            (144, 151, "shortUnconditionalJump"),
            (152, 159, "shortConditionalJump"),
            (160, 167, "longUnconditionalJump"),
            (168, 171, "longJumpIfTrue"),
            (172, 175, "longJumpIfFalse"),
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


def initialize_bytecode_names():
    result = [None] * 256
    for entry in BYTECODE_RANGES:
        if len(entry) == 2:
            positions = [entry[0]]
        else:
            positions = range(entry[0], entry[1]+1)
        for pos in positions:
            result[pos] = entry[-1]
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

from rpython.rlib.unroll import unrolling_iterable
unrolling_ranges = unrolling_iterable(BYTECODE_RANGES)
def bytecode_step_translated(self, context):
    bytecode = context.getbytecode()
    for entry in unrolling_ranges:
        if len(entry) == 2:
            bc, methname = entry
            if bytecode == bc:
                return getattr(context, methname)(self, bytecode)
        else:
            start, stop, methname = entry
            if start <= bytecode <= stop:
                return getattr(context, methname)(self, bytecode)
    assert 0, "unreachable"

Interpreter.step = bytecode_step_translated

# Smalltalk debugging facilities, patching Interpreter and ContextPartShadow
# in order to enable tracing/jumping for message sends etc.
def debugging():
    def stepping_debugger_init(original):
        def meth(self, space, image=None, image_name="", trace=False,
                max_stack_depth=constants.MAX_LOOP_DEPTH):
            return_value = original(self, space, image=image,
                                    image_name=image_name, trace=trace,
                                    max_stack_depth=max_stack_depth)
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
            def over(): options[0] = True; print  'Skipping #%s. You still need to continue(c).' % w_selector.as_repr_string()
            def pstack(): print s_context.print_stack()
            if interp.message_stepping:
                if argcount == 0:
                    print "-> %s %s" % (receiver.as_repr_string(),
                            w_selector.as_repr_string())
                elif argcount == 1:
                    print "-> %s %s %s" % (receiver.as_repr_string(),
                            w_selector.as_repr_string(),
                            s_context.peek(0).as_repr_string())
                else:
                    print "-> %s %s %r" % (receiver.as_repr_string(),
                            w_selector.as_repr_string(),
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
        def meth(self, code, interp, argcount, s_method, w_selector):
            try:
                original(self, code, interp, argcount, s_method, w_selector)
            except primitives.PrimitiveFailedError, e:
                if interp.halt_on_failing_primitives:
                    func = primitives.prim_holder.prim_table[code]
                    if func.func_name != 'raise_failing_default' and code != 83:
                        import pdb; pdb.set_trace()
                        try:
                            func(interp, self, argcount, s_method) # will fail again
                        except primitives.PrimitiveFailedError:
                            pass
                raise e
        return meth

    ContextPartShadow._call_primitive = stepping_debugger_failed_primitive_halt(ContextPartShadow._call_primitive)

    def trace_missing_named_primitives(original):
        def meth(interp, s_frame, argcount, s_method=None):
            try:
                return original(interp, s_frame, argcount, s_method=s_method)
            except primitives.PrimitiveFailedError, e:
                space = interp.space
                w_description = s_method.w_self().literalat0(space, 1)
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
