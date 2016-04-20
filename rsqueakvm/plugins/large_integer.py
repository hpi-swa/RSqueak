from rsqueakvm.primitives import prim_table, \
    BIT_AND, BIT_OR, BIT_XOR, BIT_SHIFT, ADD, SUBTRACT, DIVIDE, MULTIPLY, \
    pos_32bit_int
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin

LargeIntegerPlugin = Plugin()


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
        LargeIntegerPlugin.expose_primitive(clean_stack=False, no_result=True)(func)
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
        LargeIntegerPlugin.expose_primitive()(func)
    make_func(primitive)

ops = {
    'primDigitAdd': ADD,
    'primDigitSubtract': SUBTRACT,
}
for name, primitive in ops.items():
    def make_func(primitive):
        primfunc = prim_table[primitive]
        def func(interp, s_frame, argcount):
            """This operation is only supported for sign-matching
            receiver/argument pairs."""
            if 2 < argcount or argcount < 1:
                raise PrimitiveFailedError
            rcvr = s_frame.peek(argcount).unwrap_longlong(interp.space)
            arg =  s_frame.peek(argcount - 1).unwrap_longlong(interp.space)
            if (rcvr  < 0 and arg >= 0) or (rcvr >= 0 and arg < 0):
                raise PrimitiveFailedError
            return primfunc(interp, s_frame, argcount)
        func.func_name = name
        LargeIntegerPlugin.expose_primitive(clean_stack=False, no_result=True)(func)
    make_func(primitive)
