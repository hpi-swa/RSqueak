import py
import os
import math
import time

from rsqueakvm import storage_contexts, constants, wrapper, display
from rsqueakvm.model.base import W_Object
from rsqueakvm.model.character import W_Character
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod
from rsqueakvm.model.display import W_DisplayBitmap
from rsqueakvm.model.numeric import (W_Float, W_SmallInteger,
                                     W_LargeInteger)
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm import primitives
from rsqueakvm.primitives import prim_table
from rsqueakvm.primitives.constants import *

from rpython.rlib.rarithmetic import intmask, r_uint, r_int64
from rpython.rlib.rfloat import isinf, isnan

from .util import (create_space, copy_to_module, cleanup_module,
                   InterpreterForTest, very_slow_test)


def setup_module():
    from rpython.tool import ansi_print
    ansi_print.isatty = lambda: False
    space = create_space(bootstrap=True)
    wrap = space.w
    bootstrap_class = space.bootstrap_class
    new_frame = space.make_frame
    old_suspended_context = wrapper.ProcessWrapper.store_suspended_context
    wrapper.ProcessWrapper.store_suspended_context = lambda s, x: s
    copy_to_module(locals(), __name__)

def teardown_module():
    wrapper.ProcessWrapper.store_suspended_context = old_suspended_context
    cleanup_module(__name__)

class MockFrame(W_PointersObject):
    def __init__(self, space, stack):
        size = 6 + len(stack) + 6
        self._initialize_storage(space, space.w_BlockContext, size)
        self.store_all(space, [None] * 6 + stack + [space.w_nil] * 6)
        s_self = self.as_context_get_shadow(space)
        s_self.reset_stack()
        s_self.push_all(stack)
        s_self.store_expected_argument_count(0)

    def as_context_get_shadow(self, space):
        if not isinstance(self.strategy, storage_contexts.ContextPartShadow):
            self.strategy = storage_contexts.ContextPartShadow(space, self, self.size(), space.w_BlockContext)
            self.strategy.init_temps_and_stack()
        return self.strategy

def mock(space, stack, context=None):
    mapped_stack = [space.w(x) for x in stack]
    if context is None:
        frame = MockFrame(space, mapped_stack)
    else:
        frame = context
        for i in range(len(stack)):
            frame.as_context_get_shadow(space).push(stack[i])
    interp = InterpreterForTest(space)
    return interp, frame, len(stack)

def _prim(space, code, stack, context=None):
    interp, w_frame, argument_count = mock(space, stack, context)
    prim_table[code](interp, w_frame.as_context_get_shadow(space), argument_count-1)
    res = w_frame.as_context_get_shadow(space).pop()
    s_frame = w_frame.as_context_get_shadow(space)
    assert not s_frame.stackdepth() - s_frame.tempsize()  # check args are consumed
    return res

def prim(code, stack, context=None):
    return _prim(space, code, stack, context)

def prim_fails(code, stack):
    interp, w_frame, argument_count = mock(space, stack)
    orig_stack = list(w_frame.as_context_get_shadow(space).stack())
    with py.test.raises(PrimitiveFailedError):
        prim_table[code](interp, w_frame.as_context_get_shadow(space), argument_count - 1)
    assert w_frame.as_context_get_shadow(space).stack() == orig_stack

# smallinteger tests
def test_small_int_add():
    assert prim(ADD, [1,2]).value == 3
    assert prim(ADD, [3,4]).value == 7
    assert prim(ADD, [constants.TAGGED_MAXINT, 2]).value == constants.TAGGED_MAXINT + 2
    if constants.LONG_BIT == 32:
        assert prim(ADD, [constants.MAXINT, 2]).unwrap_long_untranslated(space) == constants.MAXINT + 2
        assert prim(ADD, [2 * constants.MAXINT - 2, 2]).unwrap_long_untranslated(space) == 2 * constants.MAXINT
    else:
        assert r_uint(prim(ADD, [constants.MAXINT, constants.MAXINT]).unwrap_long_untranslated(space)) == constants.MAXINT * 2

def test_small_int_minus():
    assert prim(SUBTRACT, [5,9]).value == -4

def test_small_int_multiply():
    assert prim(MULTIPLY, [6,3]).value == 18
    if constants.LONG_BIT == 32:
        w_result = prim(MULTIPLY, [constants.MAXINT, 2])
        assert isinstance(w_result, W_LargeInteger)
        assert r_uint(w_result.unwrap_long_untranslated(space)) == constants.MAXINT * 2
    else:
        w_result = prim(MULTIPLY, [constants.MAXINT, constants.MAXINT])
        assert isinstance(w_result, W_LargeInteger)
        assert w_result.unwrap_long_untranslated(space) == constants.MAXINT ** 2

def test_small_int_divide():
    assert prim(DIVIDE, [6,3]).value == 2

def test_small_int_divide_fail():
    prim_fails(DIVIDE, [12, 0])
    prim_fails(DIVIDE, [12, 7])

def test_small_int_mod():
    assert prim(MOD, [12,7]).value == 5

def test_small_int_mod_fail():
    prim_fails(MOD, [12, 0])

def test_small_int_div():
    assert prim(DIV, [12,3]).value == 4
    assert prim(DIV, [12,7]).value == 1

def test_small_int_div_fail():
    prim_fails(DIV, [12, 0])

def test_small_int_quo():
    assert prim(QUO, [12,3]).value == 4
    assert prim(QUO, [12,7]).value == 1
    assert prim(QUO, [-9,4]).value == -2
    assert prim(QUO, [-12,12]).value == -1
    assert prim(QUO, [-12,11]).value == -1
    assert prim(QUO, [-12,13]).value == 0
    assert prim(QUO, [-12,-12]).value == 1
    assert prim(QUO, [12,-11]).value == -1
    assert prim(QUO, [12,-13]).value == 0

def test_small_int_quo_fail():
    prim_fails(QUO, [12, 0])

def test_small_int_bit_and():
    assert prim(BIT_AND, [2, 4]).value == 0
    assert prim(BIT_AND, [2, 3]).value == 2
    assert prim(BIT_AND, [3, 4]).value == 0
    assert prim(BIT_AND, [4, 4]).value == 4

def test_small_int_bit_or():
    assert prim(BIT_OR, [2, 4]).value == 6
    assert prim(BIT_OR, [2, 3]).value == 3
    assert prim(BIT_OR, [3, 4]).value == 7
    assert prim(BIT_OR, [4, 4]).value == 4

def test_small_int_bit_xor():
    assert prim(BIT_XOR, [2, 4]).value == 6
    assert prim(BIT_XOR, [2, 3]).value == 1
    assert prim(BIT_XOR, [3, 4]).value == 7
    assert prim(BIT_XOR, [4, 4]).value == 0

def test_small_int_bit_shift():
    assert prim(BIT_SHIFT, [0, -3]).value == 0
    assert prim(BIT_SHIFT, [0, -2]).value == 0
    assert prim(BIT_SHIFT, [0, -1]).value == 0
    assert prim(BIT_SHIFT, [0, 0]).value == 0
    assert prim(BIT_SHIFT, [0, 1]).value == 0
    assert prim(BIT_SHIFT, [0, 2]).value == 0
    assert prim(BIT_SHIFT, [0, 3]).value == 0

def test_small_int_bit_shift_positive():
    assert prim(BIT_SHIFT, [4, -3]).value == 0
    assert prim(BIT_SHIFT, [4, -2]).value == 1
    assert prim(BIT_SHIFT, [4, -1]).value == 2
    assert prim(BIT_SHIFT, [4, 0]).value == 4
    assert prim(BIT_SHIFT, [4, 1]).value == 8
    assert prim(BIT_SHIFT, [4, 2]).value == 16
    assert prim(BIT_SHIFT, [4, 3]).value == 32
    assert prim(BIT_SHIFT, [4, 27]).value == 536870912

def test_small_int_bit_shift_negative():
    assert prim(BIT_SHIFT, [-4, -3]).value == -1
    assert prim(BIT_SHIFT, [-4, -2]).value == -1
    assert prim(BIT_SHIFT, [-4, -1]).value == -2
    assert prim(BIT_SHIFT, [-4, 0]).value == -4
    assert prim(BIT_SHIFT, [-4, 1]).value == -8
    assert prim(BIT_SHIFT, [-4, 2]).value == -16
    assert prim(BIT_SHIFT, [-4, 3]).value == -32
    assert prim(BIT_SHIFT, [-4, 27]).value == -536870912

def test_small_int_bit_shift_overflow():
    w_result = prim(BIT_SHIFT, [4, constants.LONG_BIT])
    assert isinstance(w_result, W_LargeInteger)
    assert w_result.unwrap_long_untranslated(space) == 4 << constants.LONG_BIT
    w_result = prim(BIT_SHIFT, [4, constants.LONG_BIT - 1])
    assert isinstance(w_result, W_LargeInteger)
    assert w_result.unwrap_long_untranslated(space) == 4 << (constants.LONG_BIT - 1)
    w_result = prim(BIT_SHIFT, [4, -constants.LONG_BIT])
    assert isinstance(w_result, W_SmallInteger)
    assert w_result.value == 0
    w_result = prim(BIT_SHIFT, [-4, -constants.LONG_BIT])
    assert isinstance(w_result, W_SmallInteger)
    assert w_result.value == -1
    w_result = prim(BIT_SHIFT, [-2**(constants.LONG_BIT*2), -constants.LONG_BIT])
    assert isinstance(w_result, W_LargeInteger)
    assert w_result.unwrap_long_untranslated(space) == -2**constants.LONG_BIT
    w_result = prim(BIT_SHIFT, [-2**(constants.LONG_BIT*2), -constants.LONG_BIT - 1])
    assert isinstance(w_result, W_SmallInteger)
    assert w_result.unwrap_long_untranslated(space) == -2**(constants.LONG_BIT-1)
    w_result = prim(BIT_SHIFT, [4, constants.LONG_BIT - 3])
    assert isinstance(w_result, W_LargeInteger)
    assert w_result.unwrap_long_untranslated(space) == 4 << constants.LONG_BIT - 3

def test_smallint_as_float():
    assert prim(SMALLINT_AS_FLOAT, [12]).value == 12.0

def test_float_add():
    assert prim(FLOAT_ADD, [1.0,2.0]).value == 3.0
    assert prim(FLOAT_ADD, [3.0,4.5]).value == 7.5

def test_float_subtract():
    assert prim(FLOAT_SUBTRACT, [1.0,2.0]).value == -1.0
    assert prim(FLOAT_SUBTRACT, [15.0,4.5]).value == 10.5

def test_float_multiply():
    assert prim(FLOAT_MULTIPLY, [10.0,2.0]).value == 20.0
    assert prim(FLOAT_MULTIPLY, [3.0,4.5]).value == 13.5

def test_float_divide():
    assert prim(FLOAT_DIVIDE, [1.0,2.0]).value == 0.5
    assert prim(FLOAT_DIVIDE, [3.5,4.0]).value == 0.875

def test_float_truncate():
    assert prim(FLOAT_TRUNCATED, [-4.6]).value == -4
    assert prim(FLOAT_TRUNCATED, [-4.5]).value == -4
    assert prim(FLOAT_TRUNCATED, [-4.4]).value == -4
    assert prim(FLOAT_TRUNCATED, [4.4]).value == 4
    assert prim(FLOAT_TRUNCATED, [4.5]).value == 4
    assert prim(FLOAT_TRUNCATED, [4.6]).value == 4

def test_float_times_two_power():
    assert prim(FLOAT_TIMES_TWO_POWER, [2.0, 10]).value == 2.0 ** 11
    assert prim(FLOAT_TIMES_TWO_POWER, [-213.0, 1020]).value == float('-inf')
    assert prim(FLOAT_TIMES_TWO_POWER, [213.0, 1020]).value == float('inf')

def test_at():
    w_obj = bootstrap_class(0, varsized=True).as_class_get_shadow(space).new(1)
    foo = wrap("foo")
    w_obj.store(space, 0, foo)
    assert prim(AT, [w_obj, 1]) is foo

    w_obj = W_Float(1.1)
    foo = wrap(1)
    w_obj.store(space, 0, foo)
    assert prim(AT, [w_obj, 1]) == foo

def test_invalid_at():
    w_obj = bootstrap_class(0).as_class_get_shadow(space).new()
    prim_fails(AT, [w_obj, 1])

def test_at_put():
    w_obj = bootstrap_class(0, varsized=1).as_class_get_shadow(space).new(1)
    assert prim(AT_PUT, [w_obj, 1, 22]).value == 22
    assert prim(AT, [w_obj, 1]).value == 22

def test_at_and_at_put_bytes():
    w_str = wrap("abc")
    prim_fails(AT_PUT, [w_str, 1, "d"])
    assert prim(AT_PUT, [w_str, 1, ord('d')]).value == ord('d')
    assert prim(AT, [w_str, 1]).value == ord('d')
    assert prim(AT, [w_str, 2]).value == ord('b')
    assert prim(AT, [w_str, 3]).value == ord('c')

def test_invalid_at_put():
    w_obj = bootstrap_class(0).as_class_get_shadow(space).new()
    prim_fails(AT_PUT, [w_obj, 1, 22])

def test_size():
    w_obj = bootstrap_class(0, varsized=True).as_class_get_shadow(space).new(0)
    assert prim(SIZE, [w_obj]).value == 0
    w_obj = bootstrap_class(3, varsized=True).as_class_get_shadow(space).new(5)
    assert prim(SIZE, [w_obj]).value == 5

def test_size_of_compiled_method():
    literalsize = 3
    bytecount = 3
    w_cm = W_PreSpurCompiledMethod(space, bytecount)
    w_cm.literalsize = literalsize
    assert prim(SIZE, [w_cm]).value == (literalsize+1)*constants.BYTES_PER_WORD + bytecount

def test_string_at():
    assert prim(STRING_AT, ["foobar", 4]) == wrap("b")

def test_string_at_put():
    test_str = wrap("foobar")
    assert prim(STRING_AT_PUT, [test_str, 4, "c"]) == wrap("c")
    exp = "foocar"
    for i in range(len(exp)):
        assert prim(STRING_AT, [test_str, i]) == wrap(exp[i])

def test_invalid_object_at():
    prim_fails(OBJECT_AT, ["q", constants.CHARACTER_VALUE_INDEX+2])

def test_invalid_object_at_put():
    w_obj = bootstrap_class(1).as_class_get_shadow(space).new()
    prim_fails(OBJECT_AT_PUT, [w_obj, 2, 42])

def test_string_at_put():
    test_str = wrap("foobar")
    assert prim(STRING_AT_PUT, [test_str, 4, "c"]) == wrap("c")
    exp = "foocar"
    for i in range(1,len(exp)+1):
        assert prim(STRING_AT, [test_str, i]) == wrap(exp[i-1])

def test_new():
    w_Object = space.classtable['w_Object']
    w_res = prim(NEW, [w_Object])
    assert w_res.getclass(space).is_same_object(w_Object)

def test_invalid_new():
    prim_fails(NEW, [space.w_String])

def test_new_with_arg():
    w_res = prim(NEW_WITH_ARG, [space.w_String, 20])
    assert w_res.getclass(space).is_same_object(space.w_String)
    assert w_res.size() == 20

def test_new_with_arg_for_non_variable_sized():
    prim_fails(NEW_WITH_ARG, [space.classtable['w_ArrayedCollection'], 10])

def test_new_with_arg_for_non_variable_sized0():
    w_res = prim(NEW_WITH_ARG, [space.classtable['w_ArrayedCollection'], 0])
    assert w_res.getclass(space).is_same_object(space.classtable['w_ArrayedCollection'])
    assert w_res.size() == 0

def test_invalid_new_with_arg():
    w_Object = space.classtable['w_Object']
    prim_fails(NEW_WITH_ARG, [w_Object, 20])

def test_inst_var_at():
    # n.b.: 1-based indexing!
    w_v = prim(INST_VAR_AT,
               ["q", constants.CHARACTER_VALUE_INDEX+1])
    assert w_v.value == ord("q")

def test_inst_var_at_invalid():
    # n.b.: 1-based indexing! (and an invalid index)
    prim_fails(INST_VAR_AT, ["q", constants.CHARACTER_VALUE_INDEX+2])

def test_inst_var_at_put():
    # n.b.: 1-based indexing!
    w_q = space.w_Character.as_class_get_shadow(space).new()
    vidx = constants.CHARACTER_VALUE_INDEX+1
    ordq = ord("q")
    assert prim(INST_VAR_AT, [w_q, vidx]).is_nil(space)
    assert prim(INST_VAR_AT_PUT, [w_q, vidx, ordq]).value == ordq
    assert prim(INST_VAR_AT, [w_q, vidx]).value == ordq

def test_inst_var_at_put_invalid():
    # n.b.: 1-based indexing! (and an invalid index)
    prim_fails(INST_VAR_AT_PUT,
               ["q", constants.CHARACTER_VALUE_INDEX+2, "t"])

def test_slot_at():
    # n.b.: 1-based indexing!
    w_v = prim(SLOT_AT,
               ["q", constants.CHARACTER_VALUE_INDEX+1])
    assert w_v.value == ord("q")

def test_slot_at_invalid():
    # n.b.: 1-based indexing! (and an invalid index)
    prim_fails(SLOT_AT, ["q", constants.CHARACTER_VALUE_INDEX+2])

def test_slot_at_put():
    # n.b.: 1-based indexing!
    w_q = space.w_Character.as_class_get_shadow(space).new()
    vidx = constants.CHARACTER_VALUE_INDEX+1
    ordq = ord("q")
    assert prim(SLOT_AT, [w_q, vidx]).is_nil(space)
    assert prim(SLOT_AT_PUT, [w_q, vidx, ordq]).value == ordq
    assert prim(SLOT_AT, [w_q, vidx]).value == ordq

def test_slot_at_put_invalid():
    # n.b.: 1-based indexing! (and an invalid index)
    prim_fails(SLOT_AT_PUT,
               ["q", constants.CHARACTER_VALUE_INDEX+2, "t"])

def test_class():
    assert prim(CLASS, ["string"]).is_same_object(space.w_String)
    assert prim(CLASS, [1]).is_same_object(space.w_SmallInteger)

def test_as_oop():
    # I checked potato, and that returns the hash for as_oop
    w_obj = bootstrap_class(0).as_class_get_shadow(space).new()
    w_obj.hash = 22
    assert prim(AS_OOP, [w_obj]).value == 22

def test_as_oop_not_applicable_to_int():
    prim_fails(AS_OOP, [22])

def test_const_primitives():
    for (code, const) in [
        (PUSH_TRUE, space.w_true),
        (PUSH_FALSE, space.w_false),
        (PUSH_NIL, space.w_nil),
        (PUSH_MINUS_ONE, space.w_minus_one),
        (PUSH_ZERO, space.w_zero),
        (PUSH_ONE, space.w_one),
        (PUSH_TWO, space.w_two),
        ]:
        assert prim(code, [space.w_nil]).is_same_object(const)
    assert prim(PUSH_SELF, [space.w_nil]).is_nil(space)
    assert prim(PUSH_SELF, ["a"]).is_same_object(wrap("a"))

def test_boolean():
    assert prim(LESSTHAN, [1,2]).is_same_object(space.w_true)
    assert prim(GREATERTHAN, [3,4]).is_same_object(space.w_false)
    assert prim(LESSOREQUAL, [1,2]).is_same_object(space.w_true)
    assert prim(GREATEROREQUAL, [3,4]).is_same_object(space.w_false)
    assert prim(EQUAL, [2,2]).is_same_object(space.w_true)
    assert prim(NOTEQUAL, [2,2]).is_same_object(space.w_false)

def test_float_boolean():
    assert prim(FLOAT_LESSTHAN, [1.0,2.0]).is_same_object(space.w_true)
    assert prim(FLOAT_GREATERTHAN, [3.0,4.0]).is_same_object(space.w_false)
    assert prim(FLOAT_LESSOREQUAL, [1.3,2.6]).is_same_object(space.w_true)
    assert prim(FLOAT_GREATEROREQUAL, [3.5,4.9]).is_same_object(space.w_false)
    assert prim(FLOAT_EQUAL, [2.2,2.2]).is_same_object(space.w_true)
    assert prim(FLOAT_NOTEQUAL, [2.2,2.2]).is_same_object(space.w_false)

def test_block_copy_and_value():
    # see test_interpreter for tests of these opcodes
    return

ROUNDING_DIGITS = 8

def float_equals(w_f,f):
    return round(w_f.value,ROUNDING_DIGITS) == round(f,ROUNDING_DIGITS)

def test_primitive_square_root():
    assert prim(FLOAT_SQUARE_ROOT, [4.0]).value == 2.0
    assert float_equals(prim(FLOAT_SQUARE_ROOT, [2.0]), math.sqrt(2))
    prim_fails(FLOAT_SQUARE_ROOT, [-2.0])

def test_primitive_sin():
    assert prim(FLOAT_SIN, [0.0]).value == 0.0
    assert float_equals(prim(FLOAT_SIN, [math.pi]), 0.0)
    assert float_equals(prim(FLOAT_SIN, [math.pi/2]), 1.0)

def test_primitive_arctan():
    assert prim(FLOAT_ARCTAN, [0.0]).value == 0.0
    assert float_equals(prim(FLOAT_ARCTAN, [1]), math.pi/4)
    assert float_equals(prim(FLOAT_ARCTAN, [1e99]), math.pi/2)

def test_primitive_log_n():
    assert prim(FLOAT_LOG_N, [1.0]).value == 0.0
    assert prim(FLOAT_LOG_N, [math.e]).value == 1.0
    assert float_equals(prim(FLOAT_LOG_N, [10.0]), math.log(10))
    assert isinf(prim(FLOAT_LOG_N, [0.0]).value)  # works also for negative infinity
    assert isnan(prim(FLOAT_LOG_N, [-1.0]).value)

def test_primitive_exp():
    assert float_equals(prim(FLOAT_EXP, [-1.0]), 1/math.e)
    assert prim(FLOAT_EXP, [0]).value == 1
    assert float_equals(prim(FLOAT_EXP, [1]), math.e)
    assert float_equals(prim(FLOAT_EXP, [math.log(10)]), 10)

def equals_ttp(rcvr,arg,res):
    return float_equals(prim(FLOAT_TIMES_TWO_POWER, [rcvr,arg]), res)

def test_times_two_power():
    assert equals_ttp(1,1,2)
    assert equals_ttp(1.5,1,3)
    assert equals_ttp(2,4,32)
    assert equals_ttp(0,2,0)
    assert equals_ttp(-1,2,-4)
    assert equals_ttp(1.5,0,1.5)
    assert equals_ttp(1.5,-1,0.75)

def test_primitive_milliseconds_clock():
    start = prim(MILLISECOND_CLOCK, [0]).value
    time.sleep(0.3)
    stop = prim(MILLISECOND_CLOCK, [0]).value
    assert start + 250 <= stop

def test_signal_at_milliseconds():
    future = prim(MILLISECOND_CLOCK, [0]).value + 400
    sema = space.w_Semaphore.as_class_get_shadow(space).new()
    prim(SIGNAL_AT_MILLISECONDS, [space.w_nil, sema, future])
    assert space.objtable["w_timerSemaphore"] is sema


def test_primitive_utc_microseconds_clock():
    start = space.unwrap_int64(prim(UTC_MICROSECOND_CLOCK, [0]))
    time.sleep(0.3)
    stop = space.unwrap_int64(prim(UTC_MICROSECOND_CLOCK, [0]))
    assert start + r_int64(250 * 1000) <= stop

def test_signal_at_utc_microseconds():
    start = space.unwrap_int64(prim(UTC_MICROSECOND_CLOCK, [0]))
    future = start + r_int64(400 * 1000)
    sema = space.w_Semaphore.as_class_get_shadow(space).new()
    prim(SIGNAL_AT_UTC_MICROSECONDS, [space.w_nil, sema, future])
    assert space.objtable["w_timerSemaphore"] is sema

def test_seconds_clock():
    now = int(time.time())
    w_smalltalk_now1 = prim(SECONDS_CLOCK, [42])
    w_smalltalk_now2 = prim(SECONDS_CLOCK, [42])
    # the test now is flaky, because we assume both have the same type
    assert w_smalltalk_now2.unwrap_long_untranslated(space) - w_smalltalk_now1.unwrap_long_untranslated(space) <= 1

def test_inc_gc():
    # Should not fail :-)
    prim(INC_GC, [42])  # Dummy arg

def test_full_gc():
    # Should not fail :-)
    prim(FULL_GC, [42])  # Dummy arg

def test_interrupt_semaphore():
    prim(INTERRUPT_SEMAPHORE, [1, space.w_true])
    assert space.objtable["w_interrupt_semaphore"].is_nil(space)

    class SemaphoreInst(W_Object):
        def getclass(self, space):
            return space.w_Semaphore
    w_semaphore = SemaphoreInst()
    prim(INTERRUPT_SEMAPHORE, [1, w_semaphore])
    assert space.objtable["w_interrupt_semaphore"] is w_semaphore

def test_load_inst_var():
    " try to test the LoadInstVar primitives a little "
    w_v = prim(primitives.INST_VAR_AT_0, ["q"])
    assert w_v.value == ord("q")

def test_new_method():
    bytecode = ''.join(map(chr, [ 16, 119, 178, 154, 118, 164, 11, 112, 16, 118, 177, 224, 112, 16, 119, 177, 224, 176, 124 ]))

    shadow = bootstrap_class(0).as_class_get_shadow(space)
    w_method = prim(NEW_METHOD, [space.w_CompiledMethod, len(bytecode), 1025])
    assert w_method.literalat0(space, 0).value == 1025
    assert w_method.literalsize == 2
    assert w_method.literalat0(space, 1).is_nil(space)
    assert w_method.bytes == ["\x00"] * len(bytecode)

def test_image_name():
    space.set_system_attribute(1, "anImage.image")
    w_v = prim(IMAGE_NAME, [2])
    assert w_v.bytes == list("anImage.image")

def test_clone():
    w_obj = bootstrap_class(1, varsized=True).as_class_get_shadow(space).new(1)
    w_obj.atput0(space, 0, space.wrap_int(1))
    w_v = prim(CLONE, [w_obj])
    assert space.unwrap_int(w_v.at0(space, 0)) == 1
    w_obj.atput0(space, 0, space.wrap_int(2))
    assert space.unwrap_int(w_v.at0(space, 0)) == 1

@py.test.mark.skipif("True")
def test_change_class():
    w_obj = prim(IMAGE_NAME, [2])
    w_v = prim(CLASS, [w_obj])
    prim(CHANGE_CLASS, [w_obj, space.w_Array])
    w_v = prim(CLASS, [w_obj])

def test_primitive_system_attribute():
    assert prim(SYSTEM_ATTRIBUTE, [space.w_nil, 1337]) == space.w_nil

    space.set_system_attribute(1001, "WinuxOS")
    w_r = prim(SYSTEM_ATTRIBUTE, [space.w_nil, 1001])
    assert isinstance(w_r, W_Object)
    assert space.unwrap_string(w_r) == "WinuxOS"

def test_file_open_write(monkeypatch):
    def open_write(filename, mode, perm):
        assert filename == "nonexistant"
        assert mode == os.O_RDWR | os.O_CREAT | os.O_TRUNC
        return 42
    monkeypatch.setattr(os, "open", open_write)
    try:
        w_c = prim(FILE_OPEN, [1, space.wrap_string("nonexistant"), space.w_true])
    finally:
        monkeypatch.undo()
    assert space.unwrap_int(w_c) == 42

def test_file_open_read(monkeypatch):
    def open_read(filename, mode, perm):
        assert filename == "file"
        assert mode == os.O_RDONLY
        return 42
    monkeypatch.setattr(os, "open", open_read)
    try:
        w_c = prim(FILE_OPEN, [1, space.wrap_string("file"), space.w_false])
    finally:
        monkeypatch.undo()
    assert space.unwrap_int(w_c) == 42

def test_file_close(monkeypatch):
    def close(fd):
        assert fd == 42
    monkeypatch.setattr(os, "close", close)
    try:
        w_c = prim(FILE_CLOSE, [1, space.wrap_int(42)])
    finally:
        monkeypatch.undo()

def test_file_write(monkeypatch):
    def write(fd, string):
        assert fd == 42
        assert string == "ell"
    monkeypatch.setattr(os, "write", write)
    try:
        w_c = prim(
            FILE_WRITE,
            [1, space.wrap_int(42), space.wrap_string("hello"), space.wrap_int(2), space.wrap_int(3)]
        )
    finally:
        monkeypatch.undo()

def test_file_write_errors(monkeypatch):
    with py.test.raises(PrimitiveFailedError):
        w_c = prim(
            FILE_WRITE,
            [1, space.wrap_int(42), space.wrap_string("hello"), space.wrap_int(-1), space.wrap_int(3)]
        )
    with py.test.raises(PrimitiveFailedError):
        w_c = prim(
            FILE_WRITE,
            [1, space.wrap_int(42), space.wrap_string("hello"), space.wrap_int(2), space.wrap_int(-1)]
        )

def test_directory_delimitor():
    w_c = prim(DIRECTORY_DELIMITOR, [1])
    assert space.unwrap_char_as_byte(w_c) == os.path.sep

def test_primitive_closure_copyClosure():
    w_frame, s_frame = new_frame("<never called, but used for method generation>")
    w_outer_frame, s_initial_context = new_frame("<never called, but used for method generation>")
    w_block = prim(CLOSURE_COPY_WITH_COPIED_VALUES, map(wrap,
                    [w_outer_frame, 2, [wrap(1), wrap(2)]]), w_frame)
    assert not w_block.is_nil(space)
    assert w_block.startpc() is 5
    assert w_block.at0(space, 0) == wrap(1)
    assert w_block.at0(space, 1) == wrap(2)
    assert w_block.numArgs() is 2

# def test_primitive_string_copy():
#     w_r = prim(STRING_REPLACE, ["aaaaa", 1, 5, "ababab", 1])
#     assert w_r.unwrap_string(None) == "ababa"
#     w_r = prim(STRING_REPLACE, ["aaaaa", 1, 5, "ababab", 2])
#     assert w_r.unwrap_string(None) == "babab"
#     w_r = prim(STRING_REPLACE, ["aaaaa", 2, 5, "ccccc", 1])
#     assert w_r.unwrap_string(None) == "acccc"
#     w_r = prim(STRING_REPLACE, ["aaaaa", 2, 4, "ccccc", 1])
#     assert w_r.unwrap_string(None) == "accca"
#     prim_fails(STRING_REPLACE, ["aaaaa", 0, 4, "ccccc", 1])
#     prim_fails(STRING_REPLACE, ["aaaaa", 1, 6, "ccccc", 2])
#     prim_fails(STRING_REPLACE, ["aaaaa", 2, 6, "ccccc", 1])
#     prim_fails(STRING_REPLACE, [['a', 'b'], 1, 4, "ccccc", 1])

def build_up_closure_environment(args, copiedValues=[]):
    w_frame, s_initial_context = new_frame("<never called, but used for method generation>")

    size_arguments = len(args)
    closure = space.newClosure(w_frame, 4, #pc
                                size_arguments, copiedValues)
    s_initial_context.push_all([closure] + args)
    interp = InterpreterForTest(space)
    s_active_context = prim_table[CLOSURE_VALUE + size_arguments](interp, s_initial_context, size_arguments)
    return s_initial_context, closure, s_active_context

def test_primitive_closure_value():
    s_initial_context, closure, s_new_context = build_up_closure_environment([])

    assert s_new_context.closure is closure
    assert s_new_context.s_sender() is s_initial_context
    assert s_new_context.w_receiver().is_nil(space)

def test_primitive_closure_value_value():
    s_initial_context, closure, s_new_context = build_up_closure_environment([
            wrap("first arg"), wrap("second arg")])

    assert s_new_context.closure is closure
    assert s_new_context.s_sender() is s_initial_context
    assert s_new_context.w_receiver().is_nil(space)
    assert s_new_context.gettemp(0).unwrap_string(None) == "first arg"
    assert s_new_context.gettemp(1).unwrap_string(None) == "second arg"

def test_primitive_closure_value_value_with_temps():
    s_initial_context, closure, s_new_context = build_up_closure_environment(
            [wrap("first arg"), wrap("second arg")],
        copiedValues=[wrap('some value')])

    assert s_new_context.closure is closure
    assert s_new_context.s_sender() is s_initial_context
    assert s_new_context.w_receiver().is_nil(space)
    assert s_new_context.gettemp(0).unwrap_string(None) == "first arg"
    assert s_new_context.gettemp(1).unwrap_string(None) == "second arg"
    assert s_new_context.gettemp(2).unwrap_string(None) == "some value"

@very_slow_test
def test_primitive_some_instance():
    import gc; gc.collect()
    someInstance = map(space.wrap_list, [[1], [2]])
    w_r = prim(SOME_INSTANCE, [space.w_Array])
    assert w_r.getclass(space) is space.w_Array

@very_slow_test
def test_primitive_some_object():
    import gc; gc.collect()
    w_r = prim(SOME_OBJECT, [space.w_nil])
    assert isinstance(w_r, W_Object)

def test_primitive_next_object():
    someInstances = map(space.wrap_list, [[2], [3]])
    w_frame, s_context = new_frame("<never called, but needed for method generation>")

    s_context.push(space.w_nil)
    interp = InterpreterForTest(space)
    prim_table[SOME_OBJECT](interp, s_context, 0)
    w_1 = s_context.pop()
    assert isinstance(w_1, W_Object)

    s_context.push(w_1)
    prim_table[NEXT_OBJECT](interp, s_context, 0)
    w_2 = s_context.pop()
    assert isinstance(w_2, W_Object)
    assert w_1 is not w_2

def test_primitive_next_instance():
    someInstances = map(space.wrap_list, [[2], [3]])
    w_frame, s_context = new_frame("<never called, but needed for method generation>")

    s_context.push(space.w_Array)
    interp = InterpreterForTest(space)
    prim_table[SOME_INSTANCE](interp, s_context, 0)
    w_1 = s_context.pop()
    assert w_1.getclass(space) is space.w_Array

    s_context.push(w_1)
    prim_table[NEXT_INSTANCE](interp, s_context, 0)
    w_2 = s_context.pop()
    assert w_2.getclass(space) is space.w_Array
    assert w_1 is not w_2

def test_primitive_next_instance_wo_some_instance_in_same_frame():
    someInstances = map(space.wrap_list, [[2], [3]])
    w_frame, s_context = new_frame("<never called, but needed for method generation>")

    s_context.push(space.w_Array)
    interp = InterpreterForTest(space)
    w_1 = someInstances[0]
    assert w_1.getclass(space) is space.w_Array

    s_context.push(w_1)
    prim_table[NEXT_INSTANCE](interp, s_context, 0)
    w_2 = s_context.pop()
    assert w_2.getclass(space) is space.w_Array
    assert w_1 is not w_2

def test_primitive_value_no_context_switch(monkeypatch):
    class Context_switched(Exception):
        pass
    class Stepping(Exception):
        pass

    def quick_check_for_interrupt(s_frame, dec=1):
        raise Context_switched
    def step(s_frame):
        raise Stepping

    w_frame, s_initial_context = new_frame("<never called, but used for method generation>")

    closure = space.newClosure(w_frame, 4, 0, [])
    s_frame = w_frame.as_context_get_shadow(space)
    interp = InterpreterForTest(space)
    interp._loop = True

    try:
        monkeypatch.setattr(interp, "quick_check_for_interrupt", quick_check_for_interrupt)
        monkeypatch.setattr(interp, "step", step)
        try:
            s_frame.push(closure)
            prim_table[CLOSURE_VALUE](interp, s_frame, 0)
        except Context_switched:
            assert True
        except Stepping:
            assert False
        try:
            s_frame.push(closure)
            prim_table[CLOSURE_VALUE_NO_CONTEXT_SWITCH](interp, s_frame, 0)
        except Context_switched:
            assert False
        except Stepping:
            assert True
    finally:
        monkeypatch.undo()

def test_primitive_be_display():
    assert space.objtable["w_display"] is None
    mock_display = W_PointersObject(space, space.w_Point, 4)
    w_wordbmp = W_WordsObject(space, space.w_Bitmap, 10)
    mock_display.store(space, 0, w_wordbmp)  # bitmap
    mock_display.store(space, 1, space.wrap_int(32))  # width
    mock_display.store(space, 2, space.wrap_int(10))  # height
    mock_display.store(space, 3, space.wrap_int(1))  # depth
    prim(BE_DISPLAY, [mock_display])
    assert space.objtable["w_display"] is mock_display
    w_bitmap = mock_display.fetch(space, 0)
    assert w_bitmap is not w_wordbmp
    assert isinstance(w_bitmap, W_DisplayBitmap)
    sdldisplay = w_bitmap.display()
    assert isinstance(sdldisplay, display.SDLDisplay)

    mock_display2 = W_PointersObject(space, space.w_Point, 4)
    mock_display2.store(space, 0, W_WordsObject(space, space.w_Bitmap, 10))  # bitmap
    mock_display2.store(space, 1, space.wrap_int(32))  # width
    mock_display2.store(space, 2, space.wrap_int(10))  # height
    mock_display2.store(space, 3, space.wrap_int(1))  # depth
    prim(BE_DISPLAY, [mock_display2])
    assert space.objtable["w_display"] is mock_display2
    w_bitmap2 = mock_display.fetch(space, 0)
    assert isinstance(w_bitmap2, W_DisplayBitmap)
    assert w_bitmap.display() is w_bitmap2.display()
    assert sdldisplay.width == 32
    assert sdldisplay.height == 10

    prim(BE_DISPLAY, [mock_display])
    assert space.objtable["w_display"] is mock_display
    assert mock_display.fetch(space, 0) is w_bitmap

# def test_primitive_force_display_update(monkeypatch):
#     mock_display = W_PointersObject(space, space.w_Point, 4)
#     w_wordbmp = W_WordsObject(space, space.w_Array, 10)
#     mock_display.store(space, 0, w_wordbmp)  # bitmap
#     mock_display.store(space, 1, space.wrap_int(32))  # width
#     mock_display.store(space, 2, space.wrap_int(10))  # height
#     mock_display.store(space, 3, space.wrap_int(1))  # depth
#     prim(BE_DISPLAY, [mock_display])

#     class DisplayFlush(Exception):
#         pass

#     def flush_to_screen_mock(self, force=False):
#         raise DisplayFlush

#     try:
#         monkeypatch.setattr(space.display().__class__, "flip", flush_to_screen_mock)
#         with py.test.raises(DisplayFlush):
#             prim(FORCE_DISPLAY_UPDATE, [mock_display])
#     finally:
#         monkeypatch.undo()

def test_screen_size_queries_sdl_window_size(monkeypatch):
    class MockDisplay:
        width = 3
        height = 2
    mock_display = MockDisplay()
    monkeypatch.setattr(space, 'display', lambda: mock_display)
    mock_displayScreen_class = bootstrap_class(0)
    def assert_screen_size():
        w_screen_size = prim(SCREEN_SIZE, [mock_displayScreen_class])
        assert w_screen_size.getclass(space) is space.w_Point
        screen_size_point = wrapper.PointWrapper(space, w_screen_size)
        assert screen_size_point.x() == mock_display.width
        assert screen_size_point.y() == mock_display.height
    assert_screen_size()
    mock_display.width = 4
    mock_display.height = 3
    assert_screen_size()

def test_immediate_identity_hash():
    w_char = space.wrap_char('x')
    w_result = prim(IMMEDIATE_IDENTITY_HASH, [w_char])
    assert isinstance(w_result, W_SmallInteger)
    assert w_result.value == ord('x')
    # TODO: add assertion for w_float once 64bit Spur images are supported

def test_class_identity_hash():
    w_result = prim(CLASS_IDENTITY_HASH, [space.w_nil.getclass(space)])
    assert w_result.value == space.w_nil.getclass(space).gethash()
    s_class = bootstrap_class(0).as_class_get_shadow(space)
    w_result = prim(CLASS_IDENTITY_HASH, [s_class.w_self()])
    assert isinstance(w_result, W_SmallInteger)

def test_character_value():
    # SmallInteger>>asCharacter
    w_result = prim(CHARACTER_VALUE, [space.wrap_int(ord('x'))])
    assert w_result.value == ord('x')
    assert isinstance(w_result, W_Character)
    # Character class>>value:
    w_result = prim(CHARACTER_VALUE,
            [space.wrap_char('x').getclass(space), ord('y')])
    assert w_result.value == ord('y')

def test_primitive_context_size():
    s_initial_context, closure, s_new_context = build_up_closure_environment([
        wrap("first arg"), wrap("second arg")])

    context_size = prim(CTXT_SIZE, [s_new_context.w_self()])
    assert context_size.value is 2

def test_primitive_context_size_smallint():
    assert prim(CTXT_SIZE, [space.wrap_int(1)]).value is 0

def test_primitive_context_nil():
    assert prim(CTXT_SIZE, [space.w_nil]).value is 0

def test_numericbitblt(monkeypatch):
    # XXX this does not test, that it gets called
    def simulate(w_name, signature, interp, s_frame, argcount, w_method):
        # assert w_name.getclass(space) is space.w_String
        assert w_name.str_content() == "'primitiveCopyBits'"
        assert signature[0] == "BitBltPlugin"
        assert signature[1] == "primitiveCopyBits"
        return "ok"

    from rsqueakvm.plugins.simulation import SimulationPlugin
    monkeypatch.setattr(SimulationPlugin, "simulate", simulate)

    try:
        assert prim(BITBLT_COPY_BITS, ["myReceiver"]).str_content() == "'myReceiver'"
    finally:
        monkeypatch.undo()

# The next cannot be tested untranslated :(
# def test_primitive_byte_size_of_object():
#     assert prim(BYTE_SIZE_OF_INSTANCE, [space.w_SmallInteger]).value is 0
#     assert prim(BYTE_SIZE_OF_INSTANCE, [space.w_LargePositiveInteger]).value is 0
#     assert prim(BYTE_SIZE_OF_INSTANCE, [space.w_LargePositiveInteger, space.wrap_int(8)]).value is 0
#     assert prim(BYTE_SIZE_OF_INSTANCE, [space.w_Float]).value is 0
#     assert prim(BYTE_SIZE_OF_INSTANCE, [space.w_CompiledMethod]).value is 0
#     assert prim(BYTE_SIZE_OF_INSTANCE, [space.w_ByteArray]).value is 0
#     assert prim(BYTE_SIZE_OF_INSTANCE, [space.w_Point]).value is 0
#     assert prim(BYTE_SIZE_OF_INSTANCE, [space.w_Bitmap]).value is 0

# Note:
#   NEXT is unimplemented as it is a performance optimization
#   NEXT_PUT is unimplemented as it is a performance optimization
#   AT_END is unimplemented as it is a performance optimization
#   BLOCK_COPY is tested in test_interpreter
#   VALUE is tested in test_interpreter
#   VALUE_WITH_ARGS is tested in test_interpreter
#   OBJECT_AT is tested in test_interpreter
#   OBJECT_AT_PUT is tested in test_interpreter
