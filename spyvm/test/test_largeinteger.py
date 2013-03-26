import py
import operator
from spyvm import squeakimage, model, constants, error
from spyvm import interpreter, shadow, objspace, primitives
from spyvm.test import test_miniimage as tools
from spyvm.test.test_miniimage import perform, w
from spyvm.test.test_primitives import MockFrame

from rpython.rlib.rarithmetic import intmask, r_uint

space, interp = tools.setup_module(tools, filename='bootstrapped.image')


def find_symbol_in_methoddict_of(string, s_class):
    s_methoddict = s_class.s_methoddict()
    s_methoddict.sync_cache()
    methoddict_w = s_methoddict.methoddict
    for each in methoddict_w.keys():
        if each.as_string() == string:
            return each

def initialize_class(w_class):
    initialize_symbol = find_symbol_in_methoddict_of("initialize", 
                        w_class.shadow_of_my_class(tools.space))
    perform(w_class, initialize_symbol)

def test_initialize_string_class():
    interp.trace = False
    #initialize String class, because equality testing requires a class var set.
    initialize_class(w("string").getclass(tools.space))

def perform_primitive(rcvr, w_selector, *args):

    code = rcvr.getclass(space)._shadow.lookup(w_selector).primitive()
    assert code
    func = primitives.prim_holder.prim_table[code]
    s_frame = MockFrame([rcvr] + list(args)).as_context_get_shadow(space)
    func(interp, s_frame, len(args))
    return s_frame.pop()

def w_l(largeInteger):
    if largeInteger >= 0 and largeInteger <= constants.TAGGED_MAXINT:
        return space.wrap_int(intmask(largeInteger))
    else:
        return model.W_LargePositiveInteger1Word(intmask(largeInteger))

# test that using W_LargePositiveInteger1Word yields the correct results.
# we use this way of testing to have multiple different test which may fail
def do_primitive(selector, operation, i=None, j=None, trace=False):
    candidates = i if i is not None else [0x1FFFFFFF, 0x3FFFFFFF, 0x7FFFFFFF]

    try:
        w_selector = space.get_special_selector(selector)
    except Exception:
        w_selector = find_symbol_in_methoddict_of(selector, w(intmask(candidates[0])).getclass(space)._shadow)

    interp.trace=trace
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
    interp.trace=False

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
#    do_primitive("bitShift:", shift, j=-5)
    do_primitive("bitShift:", shift, i=[9470032], j=[6]) # 8

# def test_primitiveAdd():
#     do_primitive("+", operator.add)

# def test_primitiveSub():
#     do_primitive("-", operator.sub, j=[0xFF, 0xFFFF, 0xF0E0D0C0], i=[-1, -1, -1])
#     do_primitive("-", operator.sub)
    # do_primitive("-", operator.sub, i=[0xFF], j=0x3FFFFFFF)

