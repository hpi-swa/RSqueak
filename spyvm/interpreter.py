import py
from spyvm.shadow import ContextPartShadow, MethodContextShadow, BlockContextShadow
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

    _w_last_active_context = None
    cnt = 0
    _last_indent = ""
    jit_driver = jit.JitDriver(
        greens=['pc', 'self', 'method'],
        reds=['s_active_context', 'w_active_context'],
        get_printable_location=get_printable_location
    )
    
    def __init__(self, space, image=None, image_name="", max_stack_depth=500):
        self.space = space
        self.image = image
        self.image_name = image_name
        self.max_stack_depth = max_stack_depth
        self.stack_depth = max_stack_depth
        self._loop = False

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
        w_new_context = w_active_context
        while True:
            try:
                w_new_context = self.c_loop(w_new_context)
            except StackOverflow, e:
                self.stack_depth = self.max_stack_depth
                w_new_context = e.w_context

    def c_loop(self, w_active_context):
        s_active_context = w_active_context.as_context_get_shadow(self.space)
        while True:
            pc = s_active_context._pc
            method = s_active_context.method()

            self.jit_driver.jit_merge_point(
                pc=pc, self=self, method=method,
                s_active_context=s_active_context,
                w_active_context=w_active_context)
            w_return_to_context = self.step(s_active_context)
            if (w_return_to_context is not None
                    and not w_return_to_context.is_same_object(w_active_context)):
                w_active_context.as_context_get_shadow(self.space).mark_returned()
                return w_return_to_context

    def stack_frame(self, w_new_frame):
        if not self._loop:
            return w_new_frame # this test is done to not loop in test,
                               # but rather step just once where wanted
        if self.stack_depth == 1:
            raise StackOverflow(w_new_frame)

        self.stack_depth = self.stack_depth - 1
        retval = self.c_loop(w_new_frame)
        self.stack_depth = self.stack_depth + 1
        return retval

    def perform(self, w_receiver, selector, *arguments_w):
        if isinstance(selector, str):
            if selector == "asSymbol":
                w_selector = self.image.w_asSymbol
            else:
                w_selector = self.perform(self.space.wrap_string(selector), "asSymbol")
        else:
            w_selector = selector

        w_method = model.W_CompiledMethod()
        w_method.setbytes([chr(124)]) #returnTopFromMethod
        s_method = w_method.as_compiledmethod_get_shadow(self.space)
        s_frame = MethodContextShadow.make_context(
                self.space, s_method, w_receiver, [], None).get_shadow(self.space)
        s_frame.push(w_receiver)
        s_frame.push_all(list(arguments_w))
        try:
            w_new_frame = s_frame._sendSelfSelector(w_selector, len(arguments_w), self)
            if w_new_frame == None:
                # which means that we tried to call a primitive method
                return s_frame.pop()
            else:
                self.loop(w_new_frame)
        except ReturnFromTopLevel, e:
            return e.object

class ReturnFromTopLevel(Exception):
    def __init__(self, object):
        self.object = object

class StackOverflow(Exception):
    def __init__(self, w_top_context):
        self.w_context = w_top_context

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
        self.push(self.method().getliteral(index))

    def pushLiteralVariableBytecode(self, interp, current_bytecode):
        # this bytecode assumes that literals[index] is an Association
        # which is an object with two named vars, and fetches the second
        # named var (the value).
        index = current_bytecode & 31
        w_association = self.method().getliteral(index)
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
        w_selector = self.method().getliteral(current_bytecode & 15)
        argcount = ((current_bytecode >> 4) & 3) - 1
        return self._sendSelfSelector(w_selector, argcount, interp)

    def _sendSelfSelector(self, w_selector, argcount, interp):
        receiver = self.peek(argcount)
        return self._sendSelector(w_selector, argcount, interp,
                                  receiver, receiver.shadow_of_my_class(self.space))

    def _sendSuperSelector(self, w_selector, argcount, interp):
        w_compiledin = self.method().w_compiledin
        assert isinstance(w_compiledin, model.W_PointersObject)
        s_compiledin = w_compiledin.as_class_get_shadow(self.space)
        return self._sendSelector(w_selector, argcount, interp, self.w_receiver(),
                                  s_compiledin.s_superclass())

    def _sendSelector(self, w_selector, argcount, interp,
                      receiver, receiverclassshadow):
        if interp.should_trace():
            print "%sSending selector %r to %r with: %r" % (
                interp._last_indent, w_selector.as_string(), receiver,
                [self.peek(argcount-1-i) for i in range(argcount)])
        assert argcount >= 0
        s_method = receiverclassshadow.lookup(w_selector)
        # XXX catch MethodNotFound here and send doesNotUnderstand:
        # AK shouln't that be done in lookup itself, please check what spec says about DNU in case of super sends.
        if s_method.primitive:
            # the primitive pushes the result (if any) onto the stack itself
            code = s_method.primitive
            if interp.should_trace():
                print "%sActually calling primitive %d" % (interp._last_indent, code,)
            func = primitives.prim_table[code]
            try:
                # note: argcount does not include rcvr
                return func(interp, self, argcount)
            except primitives.PrimitiveFailedError:
                if interp.should_trace(True):
                    print "PRIMITIVE FAILED: %d %s" % (s_method.primitive, w_selector.as_string(),)
                pass # ignore this error and fall back to the Smalltalk version
        arguments = self.pop_and_return_n(argcount)
        w_frame = s_method.create_frame(self.space, receiver, arguments,
                                      self.w_self())
        self.pop()
        return interp.stack_frame(w_frame)

    def _return(self, return_value, interp, w_return_to):
        # for tests, when returning from the top-level context
        if w_return_to.is_same_object(self.space.w_nil):
            raise ReturnFromTopLevel(return_value)
        # make this context a returned one
        self.mark_returned()

        w_return_to.as_context_get_shadow(self.space).push(return_value)
        return w_return_to

    def returnReceiver(self, interp, current_bytecode):
        return self._return(self.w_receiver(), interp, self.s_home().w_sender())

    def returnTrue(self, interp, current_bytecode):
        return self._return(interp.space.w_true, interp, self.s_home().w_sender())

    def returnFalse(self, interp, current_bytecode):
        return self._return(interp.space.w_false, interp, self.s_home().w_sender())

    def returnNil(self, interp, current_bytecode):
        return self._return(interp.space.w_nil, interp, self.s_home().w_sender())

    def returnTopFromMethod(self, interp, current_bytecode):
        # overwritten in MethodContextShadow
        return self._return(self.top(), interp, self.s_home().w_sender())

    def returnTopFromBlock(self, interp, current_bytecode):
        return self._return(self.top(), interp, self.w_sender())

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
            self.push(self.method().getliteral(variableIndex))
        elif variableType == 3:
            w_association = self.method().getliteral(variableIndex)
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
            w_association = self.method().getliteral(variableIndex)
            association = wrapper.AssociationWrapper(self.space, w_association)
            association.store_value(self.top())

    def extendedStoreAndPopBytecode(self, interp, current_bytecode):
        self.extendedStoreBytecode(interp, current_bytecode)
        self.pop()

    def getExtendedSelectorArgcount(self):
        descriptor = self.getbytecode()
        return ((self.method().getliteral(descriptor & 31)),
                (descriptor >> 5))

    def singleExtendedSendBytecode(self, interp, current_bytecode):
        w_selector, argcount = self.getExtendedSelectorArgcount()
        return self._sendSelfSelector(w_selector, argcount, interp)

    def doubleExtendedDoAnythingBytecode(self, interp, current_bytecode):
        second = self.getbytecode()
        third = self.getbytecode()
        opType = second >> 5
        if opType == 0:
            # selfsend
            return self._sendSelfSelector(self.method().getliteral(third),
                                          second & 31, interp)
        elif opType == 1:
            # supersend
            return self._sendSuperSelector(self.method().getliteral(third),
                                           second & 31, interp)
        elif opType == 2:
            # pushReceiver
            self.push(self.w_receiver().fetch(self.space, third))
        elif opType == 3:
            # pushLiteralConstant
            self.push(self.method().getliteral(third))
        elif opType == 4:
            # pushLiteralVariable
            w_association = self.method().getliteral(third)
            association = wrapper.AssociationWrapper(self.space, w_association)
            self.push(association.value())
        elif opType == 5:
            self.w_receiver().store(self.space, third, self.top())
        elif opType == 6:
            self.w_receiver().store(self.space, third, self.pop())
        elif opType == 7:
            w_association = self.method().getliteral(third)
            association = wrapper.AssociationWrapper(self.space, w_association)
            association.store_value(self.top())

    def singleExtendedSuperBytecode(self, interp, current_bytecode):
        w_selector, argcount = self.getExtendedSelectorArgcount()
        return self._sendSuperSelector(w_selector, argcount, interp)

    def secondExtendedSendBytecode(self, interp, current_bytecode):
        descriptor = self.getbytecode()
        w_selector = self.method().getliteral(descriptor & 63)
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
        w_closure = space.newClosure(self._w_self, self.pc(), numArgs, 
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


BYTECODE_TABLE = initialize_bytecode_table()


def make_bytecode_dispatch_translated():
    # this is a performance optimization: when translating the
    # interpreter, the bytecode dispatching is not implemented as a
    # list lookup and an indirect call but as a switch.

    code = ["def bytecode_step_translated(self, context):"]
    code.append("    bytecode = context.getbytecode()")
    prefix = ""
    for entry in BYTECODE_RANGES:
        if len(entry) == 2:
            numbers = [entry[0]]
        else:
            numbers = range(entry[0], entry[1]+1)
        cond = " or ".join(["bytecode == %s" % (i, )
                                for i in numbers])
        code.append("    %sif %s:" % (prefix, cond, ))
        code.append("        return context.%s(self, bytecode)" % (entry[-1], ))
        prefix = "el"
    code.append("bytecode_step_translated._always_inline_ = True")
    source = py.code.Source("\n".join(code))
    print source
    miniglob = {}
    exec source.compile() in miniglob
    return miniglob["bytecode_step_translated"]
    
bytecode_step_translated = make_bytecode_dispatch_translated()

# we_are_translated returns false on top of CPython and true when
# translating the interpreter
# if objectmodel.we_are_translated():
Interpreter.step = bytecode_step_translated

