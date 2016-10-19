from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives import prim_table
from rsqueakvm.primitives.constants import *

from rpython.rlib.rbigint import rbigint, NULLRBIGINT

LargeIntegers = Plugin()

bitops = {
    'primDigitBitAnd': BIT_AND,
    'primDigitBitOr': BIT_OR,
    'primDigitBitXor': BIT_XOR,
    'primDigitBitShiftMagnitude': BIT_SHIFT,
}
for name, primitive in bitops.items():
    def make_func(name, primitive):
        primfunc = prim_table[primitive]
        def func(interp, s_frame, argcount):
            return primfunc(interp, s_frame, argcount)
        func.func_name = name
        LargeIntegers.expose_primitive(clean_stack=False, no_result=True)(func)
    make_func(name, primitive)

@LargeIntegers.expose_primitive(unwrap_spec=[rbigint, rbigint])
def primDigitAdd(interp, s_frame, rcvr, arg):
    if rcvr.sign != arg.sign: # Squeak has weird large integer semantics :(
        return interp.space.wrap_rbigint(rcvr.sub(arg))
    else:
        return interp.space.wrap_rbigint(rcvr.add(arg))

@LargeIntegers.expose_primitive(unwrap_spec=[rbigint, rbigint])
def primDigitSubtract(interp, s_frame, rcvr, arg):
    if rcvr.sign != arg.sign: # Squeak has weird large integer semantics :(
        return interp.space.wrap_rbigint(rcvr.add(arg))
    else:
        return interp.space.wrap_rbigint(rcvr.sub(arg))

@LargeIntegers.expose_primitive(unwrap_spec=[rbigint, rbigint, object])
def primDigitMultiplyNegative(interp, s_frame, rcvr, arg, neg):
    return interp.space.wrap_rbigint(rcvr.mul(arg))

@LargeIntegers.expose_primitive(unwrap_spec=[rbigint, rbigint, object])
def primDigitDivNegative(interp, s_frame, rcvr, arg, neg):
    if arg == NULLRBIGINT:
        raise PrimitiveFailedError
    return interp.space.wrap_rbigint(rcvr.div(arg))
