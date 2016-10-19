from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives import prim_table
from rsqueakvm.primitives.constants import *

from rpython.rlib.rbigint import rbigint

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

negOps = {
    'primDigitDivNegative': DIVIDE,
    'primDigitMultiplyNegative': MULTIPLY
}
for name, primitive in negOps.items():
    def make_func(primitive):
        primfunc = prim_table[primitive]
        def func(interp, s_frame, argcount):
            if argcount != 3:
                raise PrimitiveFailedError
            neg = interp.space.w_true is s_frame.pop()
            return primfunc(interp, s_frame, 2)
        func.func_name = name
        LargeIntegers.expose_primitive()(func)
    make_func(primitive)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, index1_0, int, object])
def primitiveDecrypt(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):


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
