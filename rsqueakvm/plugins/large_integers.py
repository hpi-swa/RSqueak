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
    def make_func(primitive):
        primfunc = prim_table[primitive]
        def func(interp, s_frame, argcount):
            return primfunc(interp, s_frame, argcount)
        func.func_name = name
        LargeIntegers.expose_primitive(clean_stack=False, no_result=True)(func)
    make_func(primitive)

ops = {
    'primDigitAdd': rbigint.add,
    'primDigitSubtract': rbigint.sub,
}
for name, op in ops.items():
    def make_func(primitive):
        def func(interp, s_frame, rcvr, arg):
            return space.wrap_rbigint(op(rcvr, arg))
        func.func_name = name
        LargeIntegers.expose_primitive(unwrap_spec=[rbigint, rbigint])(func)
    make_func(primitive)

@LargeIntegers.expose_primitive(unwrap_spec=[rbigint, rbigint])
def primDigitMultiplyNegative(interp, s_frame, rcvr, arg, neg):
    return interp.space.wrap_rbigint(rcvr.mul(arg))

@LargeIntegers.expose_primitive(unwrap_spec=[rbigint, rbigint])
def primDigitDivNegative(interp, s_frame, rcvr, arg, neg):
    if arg == NULLRBIGINT:
        raise PrimitiveFailedError
    return interp.space.wrap_rbigint(rcvr.div(arg))
