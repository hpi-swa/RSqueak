
from spyvm.storage_contexts import ContextPartShadow
from spyvm.storage_classes import ClassShadow
from spyvm import model, primitives, wrapper, error
from spyvm.util.bitmanipulation import splitter
from rpython.rlib import objectmodel, unroll, jit

# unrolling_zero has been removed from rlib at some point.
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
            
            self.debug_bytecode(interp)
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
        except error.PrimitiveFailedError:
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
        except error.PrimitiveFailedError:
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
            raise error.FatalError("Illegal ExtendedStoreBytecode. veriableType 2.")
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
        except error.MethodNotFound:
            if w_arguments:
                self.push_all(w_arguments)
                # the arguments will be popped again in doesNotUnderstand but
                # jit compilation should be able to remove those operations
            return self._doesNotUnderstand(w_selector, argcount, interp, receiver)

        if not isinstance(w_method, model.W_CompiledMethod):
            if w_arguments:
                self.push_all(w_arguments)
            return self._sendOamSelector(interp, argcount, w_method, w_selector)

        code = w_method.primitive()
        if code:
            if w_arguments:
                self.push_all(w_arguments)
            try:
                return self._call_primitive(code, interp, argcount, w_method, w_selector)
            except error.PrimitiveFailedError:
                pass # ignore this error and fall back to the Smalltalk version
        if not w_arguments:
            w_arguments = self.pop_and_return_n(argcount)
        s_frame = w_method.create_frame(interp.space, receiver, w_arguments)
        self.pop() # receiver

        # ######################################################################
        if interp.is_tracing():
            interp.print_padded('-> ' + s_frame.short_str())

        return interp.stack_frame(s_frame, self)

    def _sendOamSelector(self, interp, argcount, w_method, w_selector):
        args = self.pop_and_return_n(argcount)
        arguments_w = interp.space.wrap_list(args)
        w_rcvr = self.pop()
        w_newrcvr = w_method

        w_newarguments = [w_selector, arguments_w, w_rcvr]

        self.push(w_newrcvr)
        return self._sendSpecialSelector(interp, w_newrcvr, "runWithIn", w_newarguments);

    @objectmodel.specialize.arg(1)
    def _sendSelfSelectorSpecial(self, selector, numargs, interp):
        w_selector = self.space.get_special_selector(selector)
        return self._sendSelfSelector(w_selector, numargs, interp)

    def _sendSpecialSelector(self, interp, receiver, special_selector, w_args=[]):
        space = jit.promote(self.space)
        w_special_selector = space.special_object("w_" + special_selector)
        s_class = receiver.class_shadow(space)

        try:
            w_method = s_class.lookup(w_special_selector)
        except error.MethodNotFound:
            if w_args:
                self.push_all(w_args)
                # the arguments will be popped again in doesNotUnderstand but
                # jit compilation should be able to remove those operations
            return self._doesNotUnderstand(w_special_selector, len(w_args), interp, receiver)

        if not isinstance(w_method, model.W_CompiledMethod):
            raise Exception("SpecialSelector can't be an Object")
        s_frame = w_method.create_frame(interp.space, receiver, w_args)
        # ######################################################################
        if interp.is_tracing():
            interp.print_padded('-> %s %s' % (special_selector, s_frame.short_str()))

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
        except error.MethodNotFound:
            s_class = receiver.class_shadow(self.space)
            assert isinstance(s_class, ClassShadow)
            raise error.Exit("Missing doesNotUnderstand in hierarchy of %s" % s_class.getname())

    def _mustBeBoolean(self, interp, receiver):
        return self._sendSpecialSelector(interp, receiver, "mustBeBoolean")
        
    def _call_primitive(self, code, interp, argcount, w_method, w_selector):
        # ##################################################################
        if interp.is_tracing() and isinstance(w_method, model.W_CompiledMethod):
            interp.print_padded("-> primitive %d \t(in %s, named %s)" % (
                                    code, self.w_method().get_identifier_string(),
                                    w_selector.selector_string()))
        func = primitives.prim_holder.prim_table[code]
        try:
            # note: argcount does not include rcvr
            # the primitive pushes the result (if any) onto the stack itself
            return func(interp, self, argcount, w_method)
        except error.PrimitiveFailedError, e:
            if interp.is_tracing() and isinstance(w_method, model.W_CompiledMethod):
                interp.print_padded("-- primitive %d FAILED\t (in %s, named %s)" % (
                            code, w_method.safe_identifier_string(), w_selector.selector_string()))
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
        else:
            s_return_to = self.s_home().s_sender()
            assert s_return_to, "No sender to return to!"
        
        from spyvm.interpreter import Return
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
            from spyvm.interpreter import Return
            try:
                self.bytecodePrimValue(interp, 0)
            except Return, ret:
                # Local return value of ensure: block is ignored
                if not ret.arrived_at_target:
                    raise ret
            finally:
                self.mark_returned()
    
    @bytecode_implementation()
    def unknownBytecode(self, interp, current_bytecode):
        raise error.MissingBytecode("unknownBytecode")

    @bytecode_implementation()
    def experimentalBytecode(self, interp, current_bytecode):
        raise error.MissingBytecode("experimentalBytecode")

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
    
    def debug_bytecode(self, interp):
        # Hook used in interpreter_debugging
        pass
    
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
