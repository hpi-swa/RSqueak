import operator
from spyvm import model, constants, primitives
from spyvm.test.test_primitives import MockFrame
from .util import read_image, copy_to_module, cleanup_module
from rpython.rlib.rarithmetic import intmask, r_uint, UINT_MAX, r_ulonglong

def setup_module():
    space, interp, _, _ = read_image('bootstrapped.image')
    space.uses_block_contexts.activate()
    w = space.w
    copy_to_module(locals(), __name__)
    interp.trace = False

def teardown_module():
    cleanup_module(__name__)

def perform_primitive(rcvr, w_selector, *args):
    code = rcvr.class_shadow(space).lookup(w_selector).primitive()
    assert code
    func = primitives.prim_holder.prim_table[code]
    s_frame = MockFrame(space, [rcvr] + list(args)).as_context_get_shadow(space)
    func(interp, s_frame, len(args))
    return s_frame.pop()

def w_l(largeInteger):
    if largeInteger >= 0 and largeInteger <= constants.TAGGED_MAXINT:
        return space.wrap_int(intmask(largeInteger))
    elif largeInteger >= 0 and largeInteger <= UINT_MAX:
        return model.W_LargePositiveInteger1Word(intmask(largeInteger))
    elif largeInteger >= 0:
        return model.W_LargePositiveInteger2Word(r_ulonglong(largeInteger))
    else:
        assert largeInteger < 0
        assert space.w_LargeNegativeInteger is not None
        w_o = model.W_BytesObject(space, space.w_LargeNegativeInteger, 4)
        w_li = model.W_LargePositiveInteger1Word(intmask(-largeInteger))
        w_o.bytes = list(w_li.unwrap_string(space))
        return w_o

# test that using W_LargePositiveInteger1Word yields the correct results.
# we use this way of testing to have multiple different test which may fail
def do_primitive(selector, operation, i=None, j=None, trace=False):
    candidates = i if i is not None else [0x1FFFFFFF, 0x3FFFFFFF, 0x7FFFFFFF]

    try:
        w_selector = space.get_special_selector(selector)
    except Exception:
        w_selector = space.find_symbol_in_methoddict(selector, w(intmask(candidates[0])).getclass(space))

    interp.trace = trace
    for i, v in enumerate(candidates):
        x = w_l(v)
        if j is None:
            y = x
        else:
            if isinstance(j, list):
                y = w_l(j[i])
            else:
                y = w_l(j)
        z = perform_primitive(x, w_selector, y)
        assert r_uint(z.value) == r_uint(operation(v, y.value))
    interp.trace = False

def test_bitAnd():
    do_primitive("bitAnd:", operator.and_)
    do_primitive("bitAnd:", operator.and_, i=[0xFFFFFFFF, 5, 0], j=[5, 0xFFFFFFFF, 0xFFFFFFFF])

def test_bitOr():
    do_primitive("bitOr:", operator.or_)
    do_primitive("bitOr:", operator.or_, i=[2424328192], j=[34])

def test_bitXor():
    do_primitive("bitXor:", operator.xor)
    do_primitive("bitXor:", operator.xor, i=[0xFFFFFFFF, 0x0F0F0F0F, 0xFFFFFF],
                                          j=[0xF0F0F0F0, 0xFFFEFCF8, 4294967295])

def test_bitShift():
    def shift(a, b):
        if b < 0:
            return a >> -b
        else:
            return a << b
    do_primitive("bitShift:", shift, i=[9470032], j=[6])
    do_primitive("bitShift:", shift, i=[94700329470032], j=[6])

def test_lessThan():
    w_selector = space.get_special_selector("<")
    assert perform_primitive(w_l(0xFFFFFFFF), w_selector, w_l(0xF0F0F0F0)) is space.w_false
    assert perform_primitive(w_l(0x0F0F0F0F), w_selector, w_l(0xFFFEFCF8)) is space.w_true
    assert perform_primitive(w_l(0xFFFFFF), w_selector, w_l(4294967295)) is space.w_true
    assert perform_primitive(w_l(-0xFFFFFFFF), w_selector, w_l(-0xF0F0F0F0)) is space.w_true
    assert perform_primitive(w_l(-0x0F0F0F0F), w_selector, w_l(-0xFFFEFCF8)) is space.w_false
    assert perform_primitive(w_l(-0xFFFFFF), w_selector, w_l(-4294967295)) is space.w_false
    assert perform_primitive(w_l(-0xFFFFFFFF), w_selector, w_l(-3)) is space.w_true
    assert perform_primitive(w_l(-0x0F0F0F0F), w_selector, w_l(0)) is space.w_true
    assert perform_primitive(w_l(-0xFFFFFF), w_selector, w_l(12)) is space.w_true

    assert perform_primitive(w_l(0xFFFFFFFFFFFFFFFF), w_selector, w_l(0xF0F0F0F0F0F0F0F0)) is space.w_false
    assert perform_primitive(w_l(0x0F0F0F0F0F0F0F0F), w_selector, w_l(0xFFFEFCF8FFFEFCF8)) is space.w_true
    assert perform_primitive(w_l(0xFFFFFFFFFFFF), w_selector, w_l(4294967295)) is space.w_true
    assert perform_primitive(w_l(-0xFFFFFFFFFFFFFFFF), w_selector, w_l(-0xF0F0F0F0F0F0F0F0)) is space.w_true
    assert perform_primitive(w_l(-0x0F0F0F0F0F0F0F0F), w_selector, w_l(-0xFFFEFCF8FFFEFCF8)) is space.w_false
    assert perform_primitive(w_l(-0xFFFFFFFFFFFF), w_selector, w_l(-4294967295)) is space.w_false
    assert perform_primitive(w_l(-0xFFFFFFFFFFFFFFFF), w_selector, w_l(-3)) is space.w_true
    assert perform_primitive(w_l(-0x0F0F0F0F0F0F0F0F), w_selector, w_l(0)) is space.w_true
    assert perform_primitive(w_l(-0xFFFFFFFFFFFF), w_selector, w_l(12)) is space.w_true
