import pytest
import py
import operator

from rsqueakvm import (interpreter, storage_classes, storage_contexts, wrapper,
                       constants, error)
from rsqueakvm.model.base import W_Object
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod
from rsqueakvm.primitives.constants import *

from .util import (create_space_interp, copy_to_module, cleanup_module,
                   import_bytecodes)


import_bytecodes(__name__)

@pytest.fixture(scope='module')
def space_and_interp():
    return create_space_interp(bootstrap=True)

@pytest.fixture
def space(space_and_interp):
    return space_and_interp[0]

@pytest.fixture
def interp(space_and_interp):
    return space_and_interp[1]

def setup_module():
    space, interp = create_space_interp(bootstrap=True)
    w = space.w
    interpret_bc = interp.interpret_bc
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

# ======= Helper methods =======

def bootstrap_class(instsize, w_superclass=None, w_metaclass=None,
                    name='?', format=storage_classes.POINTERS, varsized=True):
    return space.bootstrap_class(instsize, w_superclass, w_metaclass,
                    name, format, varsized)

def new_frame_and_args(bytes, receiver=None):
    foo, bar = w("foo"), w("bar")
    return space.make_frame(bytes, receiver=receiver, args=[foo, bar]), (foo, bar)

def new_frame(bytes, receiver=None):
    return new_frame_and_args(bytes, receiver=receiver)[0]

def step_in_interp(ctxt): # due to missing resets in between tests
    interp._loop = False
    try:
        try:
            retval = interp.step(ctxt)
            if retval is not None:
                return retval.w_self()
        except interpreter.FreshReturn, ret:
            raise ret.exception
    except interpreter.LocalReturn, ret:
        new_context = ctxt.s_sender()
        new_context.push(ret.value(interp.space))
        return new_context.w_self()
    except interpreter.NonLocalReturn, nlr:
        new_context = nlr.s_target_context
        new_context.push(nlr.value(interp.space))
        return new_context.w_self()

def assert_list(list, expected):
    for i in range(len(list)):
        exp = expected[i]
        if isinstance(exp, str):
            assert exp == list[i].unwrap_string(None)
        if not isinstance(exp, W_Object):
            exp = w(exp)
        assert list[i].is_same_object(exp)

def run_with_faked_primitive_methods(methods, func, active_context=None):

    # Install faked compiled methods that just invoke the primitive:
    for (w_class, primnum, argsize, methname) in methods:
        s_class = w_class.as_class_get_shadow(space)
        prim_meth = W_PreSpurCompiledMethod(space, 0)
        prim_meth._primitive = primnum
        prim_meth.argsize = argsize
        symbol = fakesymbol(methname)
        # somewhat evil:
        try:
            index = constants.find_selectorindex(methname)
        except ValueError:
            pass
        else:
            space.w_special_selectors.atput0(space, index, symbol)
            assert space.get_special_selector(methname) is symbol
        s_class.installmethod(symbol, prim_meth)
    try:
        func(active_context) if active_context else func()
    finally:
        # Uninstall those methods:
        for (w_class, _, _, methname) in methods:
            s_class = w_class.as_class_get_shadow(space)
            s_class.s_methoddict().sync_method_cache()

def fakesymbol(s, _cache={}):
    try:
        return _cache[s]
    except KeyError:
        result = _cache[s] = space.wrap_string(s)
        return result

def fakeliterals(space, *literals):
    def fakeliteral(lit):
        if isinstance(lit, str):
            return fakesymbol(lit)
        elif isinstance(lit, int):
            return space.wrap_int(lit)
        elif isinstance(lit, list):
            lstlen = len(lit)
            res = space.w_Array.as_class_get_shadow(space).new(lstlen)
            for i in range(lstlen):
                res.atput0(space, i, fakeliteral(lit[i]))
            return res
        return lit
    return [fakeliteral(lit) for lit in literals]

# ======= Test methods =======

def test_create_frame():
    w_method = W_PreSpurCompiledMethod(space, len("hello"))
    w_method.bytes = "hello"
    w_method.islarge = 1
    w_method.argsize = 2
    w_method._tempsize = 8
    s_frame = w_method.create_frame(space, w("receiver"), [w("foo"), w("bar")])
    w_frame = s_frame.w_self()
    assert s_frame.w_receiver().unwrap_string(None) == "receiver"
    assert s_frame.gettemp(0).unwrap_string(None) == "foo"
    assert s_frame.gettemp(1).unwrap_string(None) == "bar"
    assert s_frame.gettemp(2).is_nil(space)
    s_frame.settemp(2, w("spam"))
    assert s_frame.gettemp(2).unwrap_string(None) == "spam"
    assert s_frame.fetch_next_bytecode() == ord("h")
    assert s_frame.fetch_next_bytecode() == ord("e")
    assert s_frame.fetch_next_bytecode() == ord("l")

def test_push_pop():
    _, frame = new_frame("")
    frame.push(w(12))
    frame.push(w(34))
    frame.push(w(56))
    assert frame.peek(2).value == 12
    assert frame.pop().value == 56
    assert frame.top().value == 34
    frame.pop_n(0)
    assert frame.top().value == 34
    frame.push(w(56))
    frame.pop_n(2)
    assert frame.top().value == 12

def test_unknownBytecode():
    w_frame, s_frame = new_frame(unknownBytecode)
    py.test.raises(error.MissingBytecode, step_in_interp, s_frame)

# push bytecodes
def test_pushReceiverBytecode():
    w_frame, s_frame = new_frame(pushReceiverBytecode)
    step_in_interp(s_frame)
    assert s_frame.top().is_same_object(
            s_frame.w_receiver())

def test_pushReceiverVariableBytecode(bytecode=(pushReceiverVariableBytecode(0) +
                                                pushReceiverVariableBytecode(1) +
                                                pushReceiverVariableBytecode(2))):
    w_demo = bootstrap_class(3).as_class_get_shadow(space).new()
    egg, bar, baz = w("egg"), w("bar"), w("baz")
    w_demo.store(space, 0, egg)
    w_demo.store(space, 1, bar)
    w_demo.store(space, 2, baz)
    w_frame, s_frame = new_frame(bytecode, receiver = w_demo)
    s_frame = w_frame.as_context_get_shadow(space)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert_list(s_frame.stack(), [egg, bar, baz])

def test_pushTemporaryVariableBytecode(bytecode=(pushTemporaryVariableBytecode(0) +
                                                 pushTemporaryVariableBytecode(1) +
                                                 pushTemporaryVariableBytecode(2))):
    frames, args = new_frame_and_args(bytecode)
    w_frame = frames[0]
    foo, bar = args[0], args[1]
    s_frame = w_frame.as_context_get_shadow(space)
    temp = w("temp")
    s_frame.settemp(2, temp)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert_list(s_frame.stack(), [foo, bar, temp])

def test_pushLiteralConstantBytecode(bytecode=pushLiteralConstantBytecode(0) +
                                              pushLiteralConstantBytecode(1) +
                                              pushLiteralConstantBytecode(2)):
    w_frame, s_frame = new_frame(bytecode)
    s_frame.w_method().setliterals(fakeliterals(space, "a", "b", "c"))
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert_list(s_frame.stack(), [fakesymbol("a"), fakesymbol("b"), fakesymbol("c")])

def test_pushLiteralVariableBytecode(bytecode=pushLiteralVariableBytecode(0)):
    w_association = bootstrap_class(2).as_class_get_shadow(space).new()
    mykey, myvalue = w("mykey"), w("myvalue")
    w_association.store(space, 0, mykey)
    w_association.store(space, 1, myvalue)
    w_frame, s_frame = new_frame(bytecode)
    s_frame.w_method().setliterals(fakeliterals(space, w_association))
    step_in_interp(s_frame)
    assert_list(s_frame.stack(), [myvalue])

def test_storeAndPopReceiverVariableBytecode(bytecode=storeAndPopReceiverVariableBytecode,
                                             popped=True):
    shadow = bootstrap_class(8).as_class_get_shadow(space)
    for index in range(8):
        w_object = shadow.new()
        w_frame, s_frame = new_frame(pushConstantTrueBytecode + bytecode(index))
        s_frame.store_w_receiver(w_object)
        step_in_interp(s_frame)
        step_in_interp(s_frame)
        if popped:
            assert s_frame.stack() == []
        else:
            assert_list(s_frame.stack(), [space.w_true])

        for test_index in range(8):
            if test_index == index:
                assert w_object.fetch(space, test_index).is_same_object(space.w_true)
            else:
                assert w_object.fetch(space, test_index).is_nil(space)

def test_storeAndPopTemporaryVariableBytecode(bytecode=storeAndPopTemporaryVariableBytecode):
    for index in range(8):
        w_frame, s_frame = new_frame(pushConstantTrueBytecode + bytecode(index))
        # s_frame.temps = [None] * 8
        step_in_interp(s_frame)
        step_in_interp(s_frame)
        assert s_frame.stack() == []
        for test_index in range(8):
            if test_index == index:
                assert s_frame.gettemp(test_index).is_same_object(space.w_true)
            else:
                assert not s_frame.gettemp(test_index).is_same_object(space.w_true)

def test_pushConstantTrueBytecode():
    w_frame, s_frame = new_frame(pushConstantTrueBytecode)
    step_in_interp(s_frame)
    assert s_frame.pop().is_same_object(space.w_true)
    assert s_frame.stack() == []

def test_pushConstantFalseBytecode():
    w_frame, s_frame = new_frame(pushConstantFalseBytecode)
    step_in_interp(s_frame)
    assert s_frame.pop().is_same_object(space.w_false)
    assert s_frame.stack() == []

def test_pushConstantNilBytecode():
    w_frame, s_frame = new_frame(pushConstantNilBytecode)
    step_in_interp(s_frame)
    assert s_frame.pop().is_nil(space)
    assert s_frame.stack() == []

def test_pushConstantMinusOneBytecode():
    w_frame, s_frame = new_frame(pushConstantMinusOneBytecode)
    step_in_interp(s_frame)
    assert s_frame.pop().is_same_object(space.w_minus_one)
    assert s_frame.stack() == []

def test_pushConstantZeroBytecode():
    w_frame, s_frame = new_frame(pushConstantZeroBytecode)
    step_in_interp(s_frame)
    assert s_frame.pop().is_same_object(space.w_zero)
    assert s_frame.stack() == []

def test_pushConstantOneBytecode():
    w_frame, s_frame = new_frame(pushConstantOneBytecode)
    step_in_interp(s_frame)
    assert s_frame.pop().is_same_object(space.w_one)
    assert s_frame.stack() == []

def test_pushConstantTwoBytecode():
    w_frame, s_frame = new_frame(pushConstantTwoBytecode)
    step_in_interp(s_frame)
    assert s_frame.pop().is_same_object(space.w_two)
    assert s_frame.stack() == []

def test_pushActiveContextBytecode():
    w_frame, s_frame = new_frame(pushActiveContextBytecode)
    step_in_interp(s_frame)
    assert s_frame.pop().is_same_object(w_frame)
    assert s_frame.stack() == []

def test_duplicateTopBytecode():
    w_frame, s_frame = new_frame(pushConstantZeroBytecode + duplicateTopBytecode)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert_list(s_frame.stack(), [space.w_zero, space.w_zero])

def test_bytecodePrimBitAnd():
    w_frame, s_frame = new_frame(pushConstantOneBytecode + pushConstantTwoBytecode + bytecodePrimBitAnd)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().value == 0
    assert s_frame.stack() == []

def test_bytecodePrimBitOr():
    w_frame, s_frame = new_frame(pushConstantOneBytecode + pushConstantTwoBytecode + bytecodePrimBitOr)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().value == 3
    assert s_frame.stack() == []

def test_bytecodePrimBitShift():
    w_frame, s_frame = new_frame(pushConstantOneBytecode + pushConstantTwoBytecode + bytecodePrimBitShift)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().value == 4
    assert s_frame.stack() == []

def test_bytecodePrimClass():
    w_frame, s_frame = new_frame(pushConstantOneBytecode + bytecodePrimClass)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop() == space.w_SmallInteger
    assert s_frame.stack() == []

def test_bytecodePrimSubtract():
    w_frame, s_frame = new_frame(pushConstantOneBytecode + pushConstantTwoBytecode + bytecodePrimSubtract)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().value == -1
    assert s_frame.stack() == []

def test_bytecodePrimMultiply():
    w_frame, s_frame = new_frame(pushConstantMinusOneBytecode + pushConstantTwoBytecode + bytecodePrimMultiply)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().value == -2
    assert s_frame.stack() == []

def test_bytecodePrimDivide():
    w_frame, s_frame = new_frame(pushConstantTwoBytecode + pushConstantMinusOneBytecode + bytecodePrimDivide)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().value == -2
    assert s_frame.stack() == []

def test_bytecodePrimDiv():
    w_frame, s_frame = new_frame(pushConstantTwoBytecode + pushConstantMinusOneBytecode + bytecodePrimDiv)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().value == -2
    assert s_frame.stack() == []

def test_bytecodePrimMod():
    w_frame, s_frame = new_frame(pushConstantTwoBytecode + pushConstantMinusOneBytecode + bytecodePrimMod)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().value == 0
    assert s_frame.stack() == []

def test_bytecodePrimEquivalent():
    w_frame, s_frame = new_frame(pushConstantTwoBytecode + pushConstantMinusOneBytecode + bytecodePrimEquivalent)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().is_same_object(space.w_false)
    assert s_frame.stack() == []

    w_frame, s_frame = new_frame(pushConstantOneBytecode + pushConstantOneBytecode + bytecodePrimEquivalent)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert s_frame.pop().is_same_object(space.w_true)
    assert s_frame.stack() == []

def test_bytecodePrimNew():
    w_fakeclassclass = bootstrap_class(10, name='fakeclassclass')
    w_fakeclass = bootstrap_class(1, name='fakeclass', varsized=False,
                            w_metaclass=w_fakeclassclass)
    w_frame, s_frame = new_frame(bytecodePrimNew)
    s_frame.push(w_fakeclass)
    run_with_faked_primitive_methods(
        [[w_fakeclassclass, NEW, 0, "new"]],
        step_in_interp,
        s_frame)
    w_fakeinst = s_frame.pop()
    assert s_frame.stack() == []
    assert w_fakeinst.getclass(space).is_same_object(w_fakeclass)
    assert w_fakeinst.size() == 1

def test_bytecodePrimNewWithArg():
    w_fakeclassclass = bootstrap_class(10, name='fakeclassclass')
    w_fakeclass = bootstrap_class(1, name='fakeclass', varsized=True,
                            w_metaclass=w_fakeclassclass)
    w_frame, s_frame = new_frame(bytecodePrimNewWithArg)
    s_frame.push(w_fakeclass)
    s_frame.push(space.w_two)
    run_with_faked_primitive_methods(
        [[w_fakeclassclass, NEW_WITH_ARG, 1, "new:"]],
        step_in_interp,
        s_frame)
    w_fakeinst = s_frame.pop()
    assert s_frame.stack() == []
    assert w_fakeinst.getclass(space).is_same_object(w_fakeclass)
    assert w_fakeinst.size() == 3

def test_bytecodePrimSize():
    w_fakeclass = bootstrap_class(2, name='fakeclass', varsized=True)
    w_fakeinst = w_fakeclass.as_class_get_shadow(space).new(5)
    w_frame, s_frame = new_frame(bytecodePrimSize)
    s_frame.push(w_fakeinst)
    run_with_faked_primitive_methods(
        [[w_fakeclass, SIZE, 0, "size"]],
        step_in_interp,
        s_frame)
    assert s_frame.pop().value == 5
    assert s_frame.stack() == []

# w_class - the class from which the method is going to be called
# (and on which it is going to be installed)
# w_object - the actual object we will be sending the method to
# bytecodes - the bytecode to be executed
def sendBytecodesTest(w_class, w_object, bytecodes):
    for bytecode, result in [(returnReceiverBytecode, w_object),
          (returnTrueBytecode, space.w_true),
          (returnFalseBytecode, space.w_false),
          (returnNilBytecode, space.w_nil),
          (returnTopFromMethodBytecode, space.w_one)]:
        shadow = w_class.as_class_get_shadow(space)
        w_method = W_PreSpurCompiledMethod(space, 2)
        w_method.bytes = pushConstantOneBytecode + bytecode
        literals = fakeliterals(space, "foo")
        w_foo = literals[0]
        shadow.installmethod(w_foo, w_method)
        w_frame, s_frame = new_frame(bytecodes)
        s_frame.w_method().setliterals(literals)
        s_frame.push(w_object)
        w_active_context = step_in_interp(s_frame)
        s_active_context = w_active_context.as_context_get_shadow(space)
        assert s_active_context.w_sender() == w_frame
        assert s_active_context.stack() == []
        assert w_active_context.as_context_get_shadow(space).w_receiver().is_same_object(w_object)
        assert w_active_context.as_context_get_shadow(space).w_method().is_same_object(shadow.s_methoddict().methoddict[w_foo])
        assert s_frame.stack() == []
        step_in_interp(s_active_context)
        w_active_context = step_in_interp(s_active_context)
        s_active_context = w_active_context.as_context_get_shadow(space)
        assert w_active_context == w_frame
        assert_list(s_active_context.stack(), [result])

def test_sendLiteralSelectorBytecode():
    w_class = bootstrap_class(0)
    w_object = w_class.as_class_get_shadow(space).new()
    sendBytecodesTest(w_class, w_object, sendLiteralSelectorBytecode(0))

def test_fibWithArgument():
    bytecode=''.join(map(chr, [16, 119, 178, 154, 118, 164, 11, 112, 16, 118, 177, 224, 112, 16, 119, 177, 224, 176, 124]))
    shadow = bootstrap_class(0).as_class_get_shadow(space)
    method = W_PreSpurCompiledMethod(space, len(bytecode))
    method.literalsize = 1
    method.bytes = bytecode
    method.argsize = 1
    method._tempsize = 1
    literals = fakeliterals(space, "fib:")
    method.setliterals(literals)
    shadow.installmethod(literals[0], method)
    w_object = shadow.new()
    w_frame, s_frame = new_frame(sendLiteralSelectorBytecode(16) + returnTopFromMethodBytecode)
    s_frame.w_method().setliterals(literals)
    s_frame.push(w_object)
    s_frame.push(space.wrap_int(8))
    result = interp.interpret_toplevel(w_frame)
    assert space.unwrap_int(result) == 34

def test_send_to_primitive():

    def test():
        w_frame, s_frame = new_frame(sendLiteralSelectorBytecode(1 + 16))
        s_frame.w_method().setliterals(fakeliterals(space, "foo", "-"))
        s_frame.push(space.wrap_int(50))
        s_frame.push(space.wrap_int(8))
        w_new_frame = step_in_interp(s_frame)
        assert w_new_frame is None
        assert len(s_frame.stack()) == 1
        w_result = s_frame.pop()
        assert space.unwrap_int(w_result) == 42

    run_with_faked_primitive_methods(
        [[space.w_SmallInteger, SUBTRACT,
          1, "-"]],
        test)

def test_makePoint():
    w_frame, s_frame = new_frame(pushConstantZeroBytecode +
                             pushConstantOneBytecode +
                             bytecodePrimMakePoint)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    w_point = s_frame.top()
    point = wrapper.PointWrapper(interp.space, w_point)
    assert point.x() == 0
    assert point.y() == 1

def test_longJumpIfTrueBytecode():
    w_frame, s_frame = new_frame(longJumpIfTrueBytecode(0) + chr(15) + longJumpIfTrueBytecode(0) + chr(15))
    s_frame.push(space.w_false)
    pc = s_frame.pc() + 2
    step_in_interp(s_frame)
    assert s_frame.pc() == pc
    s_frame.push(space.w_true)
    pc = s_frame.pc() + 2
    step_in_interp(s_frame)
    assert s_frame.pc() == pc + 15

def test_longJumpIfFalseBytecode():
    w_frame, s_frame = new_frame(pushConstantTrueBytecode + longJumpIfFalseBytecode(0) + chr(15) +
                             pushConstantFalseBytecode + longJumpIfFalseBytecode(0) + chr(15))
    step_in_interp(s_frame)
    pc = s_frame.pc() + 2
    step_in_interp(s_frame)
    assert s_frame.pc() == pc
    step_in_interp(s_frame)
    pc = s_frame.pc() + 2
    step_in_interp(s_frame)
    assert s_frame.pc() == pc + 15

def test_longUnconditionalJumpBytecode():
    w_frame, s_frame = new_frame(longUnconditionalJumpBytecode(4) + chr(15))
    pc = s_frame.pc() + 2
    step_in_interp(s_frame)
    assert s_frame.pc() == pc + 15

def test_shortUnconditionalJumpBytecode():
    w_frame, s_frame = new_frame(chr(145))
    pc = s_frame.pc() + 1
    step_in_interp(s_frame)
    assert s_frame.pc() == pc + 2

def test_shortConditionalJumpBytecode():
    w_frame, s_frame = new_frame(pushConstantTrueBytecode + shortConditionalJumpBytecode(3) +
                             pushConstantFalseBytecode + shortConditionalJumpBytecode(3))
    step_in_interp(s_frame)
    pc = s_frame.pc() + 1
    step_in_interp(s_frame)
    assert s_frame.pc() == pc
    step_in_interp(s_frame)
    pc = s_frame.pc() + 1
    step_in_interp(s_frame)
    assert s_frame.pc() == pc + 4

def test_popStackBytecode():
    w_frame, s_frame = new_frame(pushConstantTrueBytecode +
                             popStackBytecode)
    step_in_interp(s_frame)
    assert_list(s_frame.stack(), [space.w_true])
    step_in_interp(s_frame)
    assert s_frame.stack() == []

def test_extendedPushBytecode():
    test_pushReceiverVariableBytecode(extendedPushBytecode + chr((0<<6) + 0) +
                                      extendedPushBytecode + chr((0<<6) + 1) +
                                      extendedPushBytecode + chr((0<<6) + 2))

    test_pushTemporaryVariableBytecode(extendedPushBytecode + chr((1<<6) + 0) +
                                       extendedPushBytecode + chr((1<<6) + 1) +
                                       extendedPushBytecode + chr((1<<6) + 2))

    test_pushLiteralConstantBytecode(extendedPushBytecode + chr((2<<6) + 0) +
                                     extendedPushBytecode + chr((2<<6) + 1) +
                                     extendedPushBytecode + chr((2<<6) + 2))

    test_pushLiteralVariableBytecode(extendedPushBytecode + chr((3<<6) + 0))

def storeAssociation(bytecode):
    w_association = bootstrap_class(2).as_class_get_shadow(space).new()
    w_association.store(space, 0, w("mykey"))
    w_association.store(space, 1, w("myvalue"))
    w_frame, s_frame = new_frame(pushConstantOneBytecode + bytecode)
    s_frame.w_method().setliterals(fakeliterals(space, w_association))
    step_in_interp(s_frame)
    step_in_interp(s_frame)
    assert w_association.fetch(space, 1).is_same_object(space.w_one)

def test_extendedStoreAndPopBytecode():
    test_storeAndPopReceiverVariableBytecode(lambda index: extendedStoreAndPopBytecode + chr((0<<6) + index))

    test_storeAndPopTemporaryVariableBytecode(lambda index: extendedStoreAndPopBytecode + chr((1<<6) + index))

    py.test.raises(error.FatalError,
                   test_storeAndPopTemporaryVariableBytecode,
                   lambda index: extendedStoreAndPopBytecode + chr((2<<6) + index))

    storeAssociation(extendedStoreAndPopBytecode + chr((3<<6) + 0))

def test_callPrimitiveAndPush_fallback():
    w_frame, s_frame = new_frame(bytecodePrimAdd)
    shadow = bootstrap_class(0).as_class_get_shadow(space)
    w_method = W_PreSpurCompiledMethod(space, 0)
    w_method.argsize = 1
    w_method._tempsize = 1
    w_method.literalsize = 1
    w_symbol = fakesymbol("+")
    shadow.installmethod(w_symbol, w_method)
    # slightly evil
    space.w_special_selectors.atput0(space, constants.find_selectorindex("+"), w_symbol)

    w_object = shadow.new()
    s_frame.push(w_object)
    s_frame.push(space.w_one)
    w_active_context = step_in_interp(s_frame)
    s_active_context = w_active_context.as_context_get_shadow(space)
    assert w_active_context.as_context_get_shadow(space).w_method() == shadow.s_methoddict().methoddict[w_symbol]
    assert s_active_context.w_receiver() is w_object
    assert w_active_context.as_context_get_shadow(space).gettemp(0).is_same_object(space.w_one)
    assert s_active_context.stack() == []

def test_bytecodePrimBool():
    w_frame, s_frame = new_frame(bytecodePrimLessThan +
                             bytecodePrimGreaterThan +
                             bytecodePrimLessOrEqual +
                             bytecodePrimGreaterOrEqual +
                             bytecodePrimEqual +
                             bytecodePrimNotEqual)
    for i in range(6):
        s_frame.push(space.w_one)
        s_frame.push(space.w_two)
        step_in_interp(s_frame)
    assert_list(s_frame.stack(), [space.w_true, space.w_false,
                                          space.w_true, space.w_false,
                                          space.w_false, space.w_true])

def test_singleExtendedSendBytecode():
    w_class = bootstrap_class(0)
    w_object = w_class.as_class_get_shadow(space).new()
    sendBytecodesTest(w_class, w_object, singleExtendedSendBytecode + chr((0<<5)+0))

def test_singleExtendedSuperBytecode(bytecode=singleExtendedSuperBytecode + chr((0<<5) + 0)):
    w_supersuper = bootstrap_class(0)
    w_super = bootstrap_class(0, w_superclass=w_supersuper)
    w_class = bootstrap_class(0, w_superclass=w_super)
    w_object = w_class.as_class_get_shadow(space).new()
    # first call method installed in w_class
    bytecodes = singleExtendedSendBytecode + chr(0)
    # which does a call to its super
    meth1 = W_PreSpurCompiledMethod(space, 2)
    meth1.bytes = pushReceiverBytecode + bytecode
    literals = fakeliterals(space, "foo")
    foo = literals[0]
    meth1.setliterals(literals)
    w_class.as_class_get_shadow(space).installmethod(foo, meth1)
    # and that one again to its super
    meth2 = W_PreSpurCompiledMethod(space, 2)
    meth2.bytes = pushReceiverBytecode + bytecode
    meth2.setliterals(fakeliterals(space, foo))
    w_super.as_class_get_shadow(space).installmethod(foo, meth2)
    meth3 = W_PreSpurCompiledMethod(space, 0)
    w_supersuper.as_class_get_shadow(space).installmethod(foo, meth3)
    w_frame, s_frame = new_frame(bytecodes)
    s_frame.w_method().setliterals(literals)
    s_frame.push(w_object)
    w_active_context = step_in_interp(s_frame)
    s_active_context = w_active_context.as_context_get_shadow(space)
    for w_specificclass in [w_super, w_supersuper]:
        w_caller_context = w_active_context
        s_caller_context = s_active_context
        step_in_interp(s_active_context)
        w_active_context = step_in_interp(s_active_context)
        s_active_context = w_active_context.as_context_get_shadow(space)
        assert s_active_context.w_sender() == w_caller_context
        assert s_active_context.stack() == []
        assert w_active_context.as_context_get_shadow(space).w_receiver() == w_object
        meth = w_specificclass.as_class_get_shadow(space).s_methoddict().methoddict[foo]
        assert s_active_context.w_method() == meth
        assert s_caller_context.stack() == []

def test_secondExtendedSendBytecode():
    w_class = bootstrap_class(0)
    w_object = w_class.as_class_get_shadow(space).new()
    sendBytecodesTest(w_class, w_object, secondExtendedSendBytecode + chr(0))

def test_doubleExtendedDoAnythinBytecode():
    w_class = bootstrap_class(0)
    w_object = w_class.as_class_get_shadow(space).new()

    sendBytecodesTest(w_class, w_object, doubleExtendedDoAnythingBytecode + chr((0<<5) + 0) + chr(0))

    test_singleExtendedSuperBytecode(doubleExtendedDoAnythingBytecode + (chr((1<<5) + 0) + chr(0)))

    test_pushReceiverVariableBytecode(doubleExtendedDoAnythingBytecode + chr(2<<5) + chr(0) +
                                      doubleExtendedDoAnythingBytecode + chr(2<<5) + chr(1) +
                                      doubleExtendedDoAnythingBytecode + chr(2<<5) + chr(2))

    test_pushLiteralConstantBytecode(doubleExtendedDoAnythingBytecode + chr(3<<5) + chr(0) +
                                     doubleExtendedDoAnythingBytecode + chr(3<<5) + chr(1) +
                                     doubleExtendedDoAnythingBytecode + chr(3<<5) + chr(2))

    test_pushLiteralVariableBytecode(doubleExtendedDoAnythingBytecode + chr(4<<5) + chr(0))

    test_storeAndPopReceiverVariableBytecode(lambda index: doubleExtendedDoAnythingBytecode + chr(5<<5) + chr(index), False)

    test_storeAndPopReceiverVariableBytecode(lambda index: doubleExtendedDoAnythingBytecode + chr(6<<5) + chr(index))

    storeAssociation(doubleExtendedDoAnythingBytecode + chr(7<<5) + chr(0))

# tests: bytecodePrimValue & bytecodePrimValueWithArg
def test_bc_3_plus_4():
    # value0
    #   " (self >> #value0) byteCode "
    #   " (self >> #value0) literals "
    #
    #   ^ [ 3 + 4 ] value
    assert interpret_bc(
        [137, 117, 200, 164, 4, 32, 33, 176, 125, 201, 124],
        fakeliterals(space, 3, 4)).value == 7


def test_bc_x_plus_x_plus_1():
    # value1
    #   " (self >> #value1) byteCode "
    #   " (self >> #value1) literals "
    #
    #   ^ [ :x | x + x + 1 ] value: 3
    assert interpret_bc(
        [137, 118, 200, 164, 7, 104, 16, 16,
         176, 118, 176, 125, 32, 202, 124],
        fakeliterals(space, 3)).value == 7

def test_bc_x_plus_y():
    # value2
    #   " (self >> #value2) byteCode "
    #   " (self >> #value2) literals "
    #
    #   ^ [ :x :y | x + y ] value: 3 value: 4

    def test():
        assert interpret_bc(
            [137, 119, 200, 164, 6, 105, 104, 16, 17,
             176, 125, 33, 34, 240, 124],
            fakeliterals(space, "value:value:", 3, 4)).value == 7
    run_with_faked_primitive_methods(
        [[space.w_BlockContext, VALUE,
          2, "value:value:"]],
        test)

def test_bc_push_rcvr_in_block():
    # value1
    #   " (self >> #value1) byteCode "
    #   " (self >> #value1) literals "
    #
    #   ^ [ self ] value
    assert interpret_bc(
        [137, 117, 200, 164, 2, 112, 125, 201, 124],
        fakeliterals(space, 3)).is_nil(space)

def test_bc_value_return():
    # valueReturn
    #   " (self >> #value1) byteCode "
    #   " (self >> #value1) literals "
    #
    #   [ ^ 1 ] value. ^ 2
    assert interpret_bc(
        [137, 117, 200, 164, 2, 118, 124, 201, 135, 119, 124],
        fakeliterals(space, )).value == 1

def test_bc_value_with_args():
    # valueWithArgs
    #   " (self >> #value1) byteCode "
    #   " (self >> #value1) literals "
    #
    #   [ :a :b | a - b ] valueWithArguments: #(3 2)
    def test():
        val = interpret_bc(
            [137, 119, 200, 164, 6,
             105, 104, 16, 17, 177,
             125, 33, 224, 124],
            fakeliterals(space, "valueWithArguments:",
                         [3, 2]))
        assert val.value == 1
    run_with_faked_primitive_methods(
        [[space.w_BlockContext, VALUE_WITH_ARGS,
          1, "valueWithArguments:"]],
        test)

def test_bc_primBytecodeAt_string():
    #   ^ 'a' at: 1
    def test():
        assert interpret_bc(
            [32, 118, 192, 124],
            fakeliterals(space, "a")) == space.wrap_char("a")
    run_with_faked_primitive_methods(
        [[space.w_String, STRING_AT, 1, "at:"]],
        test)

def test_bc_primBytecodeAtPut_string():
    #   ^ 'a' at: 1 put:'b'
    def test():
        assert interpret_bc(
            [32, 118, 33, 193, 124],
            fakeliterals(space, "a", space.wrap_char("b"))) == space.wrap_char("b")
    run_with_faked_primitive_methods(
        [[space.w_String, STRING_AT_PUT, 2, "at:put:"]],
        test)

def test_bc_primBytecodeAt_with_instvars():
    #   ^ self at: 1
    w_fakeclass = bootstrap_class(1, name='fakeclass', varsized=True)
    w_fakeinst = w_fakeclass.as_class_get_shadow(space).new(1)
    w_fakeinst.store(space, 0, space.wrap_char("a"))  # static slot 0: instance variable
    w_fakeinst.store(space, 1, space.wrap_char("b"))  # varying slot 1
    def test():
        assert space.unwrap_char_as_byte(interpret_bc(
            [112, 118, 192, 124],
            fakeliterals(space, ),
            receiver=w_fakeinst)) == "b"
    run_with_faked_primitive_methods(
        [[w_fakeclass, AT, 1, "at:"]],
        test)

def test_bc_primBytecodeAtPut_with_instvars():
    #   ^ self at: 1 put: #b
    w_fakeclass = bootstrap_class(1, name='fakeclass', varsized=True)
    w_fakeinst = w_fakeclass.as_class_get_shadow(space).new(1)
    w_fakeinst.store(space, 0, space.wrap_char("a"))  # static slot 0: instance variable
    w_fakeinst.store(space, 1, space.wrap_char("a"))  # varying slot 1
    def test():
        assert space.unwrap_char_as_byte(interpret_bc(
            [0x70, 0x76, 0x20, 0xc1, 0x7c],
            fakeliterals(space, space.wrap_char("b")),
            receiver=w_fakeinst)) == "b"
        assert space.unwrap_char_as_byte(w_fakeinst.fetch(space, 0)) == "a"
        assert space.unwrap_char_as_byte(w_fakeinst.fetch(space, 1)) == "b"
    run_with_faked_primitive_methods(
        [[w_fakeclass, AT_PUT, 2, "at:put:"]],
        test)

def test_bc_objectAtAndAtPut():
    #   ^ self objectAt: 1.          yields the method header
    #   ^ self objectAt: 2.          yields the first literal (22)
    #   ^ self objectAt: 2 put: 3.   changes the first literal to 3
    #   ^ self objectAt: 2.          yields the new first literal (3)
    prim_meth = W_PreSpurCompiledMethod(space, header=1024)
    prim_meth.setliterals(fakeliterals(space, 22))
    oal = fakeliterals(space, "objectAt:")
    oalp = fakeliterals(space, "objectAt:put:", 3)
    def test():
        assert interpret_bc(
            [112, 118, 224, 124], oal, receiver=prim_meth).value == 1024
        assert interpret_bc(
            [112, 119, 224, 124], oal, receiver=prim_meth).value == 22
        assert interpret_bc(
            [112, 119, 33, 240, 124], oalp, receiver=prim_meth).value == 3
        assert interpret_bc(
            [112, 119, 224, 124], oal, receiver=prim_meth).value == 3
    run_with_faked_primitive_methods(
        [[space.w_CompiledMethod, OBJECT_AT, 1, "objectAt:"],
         [space.w_CompiledMethod, OBJECT_AT_PUT, 2, "objectAt:put:"]],
        test)

# Closure Bytecodes
def test_bc_pushNewArrayBytecode(bytecode=pushNewArrayBytecode):
    w_frame, s_frame = new_frame(bytecode + chr(0x83))
    s_frame.push(w(fakeliterals(space, "egg")))
    s_frame.push(w(fakeliterals(space, "bar")))
    s_frame.push(w(fakeliterals(space, "baz")))
    step_in_interp(s_frame)
    array = s_frame.pop()
    assert array.size() == 3
    assert space.unwrap_array(array.at0(space, 0)) == fakeliterals(space, "egg")
    assert space.unwrap_array(array.at0(space, 1)) == fakeliterals(space, "bar")
    assert space.unwrap_array(array.at0(space, 2)) == fakeliterals(space, "baz")

def test_bc_pushNewArrayBytecode_noPopIntoArray(bytecode=pushNewArrayBytecode):
    w_frame, s_frame = new_frame(bytecode + chr(0x02))
    s_frame.push(w("egg"))
    s_frame.push(w("bar"))
    step_in_interp(s_frame)
    array = s_frame.pop()
    assert array.size() == 2
    assert array.at0(space, 0).is_nil(space)
    assert array.at0(space, 1).is_nil(space)
    assert s_frame.pop().unwrap_string(None) == "bar"
    assert s_frame.pop().unwrap_string(None) == "egg"

def test_bc_pushNewArray(bytecode=pushNewArrayBytecode):
    w_frame, s_frame = new_frame(bytecode + chr(0x07))
    step_in_interp(s_frame)
    array = s_frame.pop()
    assert array.size() == 7
    assert array.at0(space, 0).is_nil(space)

def test_bc_pushRemoteTempLongBytecode(bytecode=pushRemoteTempLongBytecode):
    w_frame, s_frame = new_frame(bytecode + chr(0) + chr(0))
    s_frame.settemp(0, space.w_Array.as_class_get_shadow(interp.space).new(2))
    step_in_interp(s_frame)
    assert s_frame.top().is_nil(space)

def setupTempArrayAndContext(bytecode):
    # both indizes are 0-relative
    w_frame, s_frame = new_frame(bytecode + chr(2) + chr(1))
    s_frame.push(w(fakeliterals(space, "english")))
    s_frame.push(w(fakeliterals(space, "bar")))
    temp_array = space.w_Array.as_class_get_shadow(interp.space).new(3)
    temp_array.atput0(space, 2, w(fakeliterals(space, "pub")))
    s_frame.settemp(1, temp_array)
    step_in_interp(s_frame)
    return s_frame, temp_array

def test_bc_pushRemoteTempLongBytecode2(bytecode=pushRemoteTempLongBytecode):
    context, _ = setupTempArrayAndContext(bytecode)
    assert space.unwrap_array(context.top()) == fakeliterals(space, "pub")

def test_bc_storeRemoteTempLongBytecode(bytecode=storeRemoteTempLongBytecode):
    context, temp_array = setupTempArrayAndContext(bytecode)
    assert space.unwrap_array(context.top()) == fakeliterals(space, "bar")
    assert space.unwrap_array(temp_array.at0(space, 2)) == fakeliterals(space, "bar")

def test_bc_storeAndPopRemoteTempLongBytecode(bytecode=storeAndPopRemoteTempLongBytecode):
    context, temp_array = setupTempArrayAndContext(bytecode)
    assert space.unwrap_array(temp_array.at0(space, 2)) == fakeliterals(space, "bar")
    assert space.unwrap_array(context.top()) == fakeliterals(space, "english")

def test_bc_pushClosureCopyCopied0ValuesBytecode(bytecode=pushClosureCopyCopiedValuesBytecode):
    for i in (0, 0xF0, 0x0FF0, 0xFFF0):
        w_frame, s_frame = new_frame(bytecode + chr(2) + chr(i >> 8) + chr(i & 0xFF))
        pc = s_frame.pc()
        # create/find a method with an appropriate blockClosure
        step_in_interp(s_frame)
        assert s_frame.pc() == pc + 4 + i
        closure = s_frame.top()
        assert closure.startpc() == pc + 4 + 4 + 1 # pc + offset + headerword + smalltalk 1-indexing
        assert closure.w_outerContext() is s_frame._w_self

def test_bc_pushClosureCopyCopied2ValuesBytecode(bytecode=pushClosureCopyCopiedValuesBytecode):
    w_frame, s_frame = new_frame(bytecode + chr(0x23) + chr(0) + chr(0))
    s_frame.push(w("english"))
    s_frame.push(w("bar"))
    pc = s_frame.pc()
    step_in_interp(s_frame)
    assert s_frame.pc() == pc + 4
    closure = s_frame.top()
    assert closure.startpc() == pc + 4 + 5
    assert closure.w_outerContext() is s_frame._w_self
    assert closure.at0(space, 0).unwrap_string(None) == "english"
    assert closure.at0(space, 1).unwrap_string(None) == "bar"

def test_blockclosure_valuevalue():
    #someTest
    #   ^ [ :a :b | a + b ] value: 1 value: 2
    def test():
        assert interpret_bc(
            [ 0x8f, 2, 0, 4, 16, 17, 0xb0, 0x7d, 0x76, 0x77, 0xf0, 0x7c ],
            fakeliterals(space, "value:value:", )).value == 3
    run_with_faked_primitive_methods(
        [[space.w_BlockClosure, CLOSURE_VALUE_VALUE,
            2, "value:value:"]],
        test)

def test_blockclosure_return():
    #someTest
    #   [ :a :b | ^ a + b ] value: 1 value: 2.
    #   ^ 1
    def test():
        assert interpret_bc(
            [ 0x8f, 2, 0, 4, 16, 17, 0xb0, 0x7c,
            0x76, 0x77, 0xf0, 0x87, 0x76, 0x7c ],
            fakeliterals(space, "value:value:", )).value == 3
    run_with_faked_primitive_methods(
        [[space.w_BlockClosure, CLOSURE_VALUE_VALUE,
            2, "value:value:"]],
        test)

def test_frame_dirty_if_active():
    bytes = reduce(operator.add, map(chr, [0x84, 0xc0, 0x00]))
    w_frame, s_frame = new_frame(bytes)
    s_frame.store_w_receiver(w_frame)
    s_frame.push(w_frame)
    s_frame.set_state(storage_contexts.ActiveContext)
    step_in_interp(s_frame)
    assert s_frame.get_state() is storage_contexts.DirtyContext

def test_frame_not_dirty_if_inactive():
    bytes = reduce(operator.add, map(chr, [0x84, 0xc0, 0x00]))
    w_frame, s_frame = new_frame(bytes)
    w_other_frame, s_other_frame = new_frame("")
    s_frame.store_w_receiver(w_other_frame)
    s_frame.push(w_frame)
    s_frame.set_state(storage_contexts.ActiveContext)
    step_in_interp(s_frame)
    assert s_frame.get_state() is storage_contexts.ActiveContext
    assert s_other_frame.get_state() is storage_contexts.InactiveContext

def test_raise_NonVirtualReturn_on_dirty_frame():
    bytes = reduce(operator.add, map(chr, [0x84, 0xc0, 0x00])) + returnTopFromMethodBytecode
    w_frame, s_frame = new_frame(bytes)
    s_frame.store_w_receiver(w_frame)
    s_frame.push(w_frame)

    interp._loop = True
    def do_test():
        interp.stack_frame(s_frame, None, True)
    py.test.raises(interpreter.NonVirtualReturn, do_test)



def test_objectsAsMethods():
    w_foo = space.wrap_string("foo")
    w_foo_ = space.wrap_string("foo:")
    w_runwithin = space.w_runWithIn

    w_holderclass = bootstrap_class(0)
    w_class = bootstrap_class(0)
    w_method = W_PreSpurCompiledMethod(space, 1)
    w_method.setbytes(str(bytearray([0x10, 0x11, 0x12, 0x8a, 0x83, 0x7c])))
    # "^ {selector. args. receiver}" push temps, pop 3 into array, return top
    w_method.argsize = 3

    classshadow = w_class.as_class_get_shadow(space)
    classshadow.installmethod(w_runwithin, w_method)
    w_object = classshadow.new()
    holdershadow = w_holderclass.as_class_get_shadow(space)
    holdershadow.installmethod(w_foo, w_object)
    holdershadow.installmethod(w_foo_, w_object)
    w_holderobject = holdershadow.new()

    bytecodes = [ 112, 208, 124 ] #pushReceiverBytecode, sendBytecode for first literal
    # ^ self foo
    literals = [ w_foo ]
    runwithin_args_w = interpret_bc(bytecodes, literals, w_holderobject)
    w_runwithin_args = space.unwrap_array(runwithin_args_w)
    assert w_runwithin_args[0] == w_foo
    assert w_runwithin_args[1].getclass(space).is_same_object(space.w_Array)
    assert w_runwithin_args[1].size() == 0 # foo has no arguments
    assert w_runwithin_args[2] == w_holderobject

    bytecodes = [0x70, 0x70, 0xE0, 0x7c] #push receiver 2x, send literal with one arg
    # ^ self foo: self
    literals = [ w_foo_ ]
    runwithin_args_w = interpret_bc(bytecodes, literals, w_holderobject)
    w_runwithin_args = space.unwrap_array(runwithin_args_w)
    assert w_runwithin_args[0] == w_foo_
    assert w_runwithin_args[1].getclass(space).is_same_object(space.w_Array)
    assert w_runwithin_args[1].size() == 1 # foo: has one argument
    assert w_runwithin_args[1].fetch(space, 0) == w_holderobject # receiver was used as argument
    assert w_runwithin_args[2] == w_holderobject

class FakeSpurImage:
    class FakeVersion:
        is_spur = True
    version = FakeVersion()

def test_skip_callPrimitive_bytecode(space, interp):
    interp.image = FakeSpurImage()
    assert interp.image.version.is_spur, "test should pretend a spur image"
    bytecodes = [139, 122, 123, 121] # callPrimitive (2 bytes args), ^true
    # note that the primitive index bytes are the bytecodes for ^false and ^nil
    result = interp.interpret_bc(bytecodes)
    assert result.is_same_object(space.w_true)
