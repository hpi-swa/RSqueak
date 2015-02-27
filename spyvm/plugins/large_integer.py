from spyvm import model
from spyvm.primitives import prim_table, \
    BIT_AND, BIT_OR, BIT_XOR, BIT_SHIFT, ADD, SUBTRACT, DIVIDE, MULTIPLY
from spyvm.error import PrimitiveFailedError
from spyvm.plugins.plugin import Plugin


LargeIntegerPlugin = Plugin()


ops = {
    'primDigitBitAnd': BIT_AND,
    'primDigitBitOr': BIT_OR,
    'primDigitBitXor': BIT_XOR,
    'primDigitBitShiftMagnitude': BIT_SHIFT,
    'primDigitAdd': ADD,
    'primDigitSubtract': SUBTRACT,
}
for (name, primitive) in ops.items():
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
for (name, primitive) in negOps.items():
    def make_func(primitive):
        primfunc = prim_table[primitive]
        def func(interp, s_frame, argcount):
            if argcount != 3:
                raise PrimitiveFailedError
            neg = interp.space.w_true is s_frame.pop()
            return primfunc(interp, s_frame, 2)
        func.func_name = name
        LargeIntegerPlugin.expose_primitive(clean_stack=False, no_result=True)(func)
    make_func(primitive)
