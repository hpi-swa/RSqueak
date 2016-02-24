from spyvm import model
from spyvm.primitives import prim_table, \
    BIT_AND, BIT_OR, BIT_XOR, BIT_SHIFT, ADD, SUBTRACT, DIVIDE, MULTIPLY, \
    pos_32bit_int
from spyvm.error import PrimitiveFailedError
from spyvm.plugins.plugin import Plugin

from rpython.rlib.rarithmetic import r_longlong, intmask, r_uint, ovfcheck
import operator

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
        LargeIntegerPlugin.expose_primitive(clean_stack=False, no_result=True)(func)
    make_func(primitive)

# cannot reuse base impl here, different unwrap
ops = {
    'primDigitAdd': operator.add,
    'primDigitSubtract': operator.sub,
}
for name, op in ops.items():
    def make_func(op):
        def func(interp, s_frame, receiver, argument):
            try:
                if isinstance(receiver, r_uint) and isinstance(argument, r_uint):
                    res = op(intmask(receiver), intmask(argument))
                    return interp.space.wrap_positive_32bit_int(intmask(res))
                elif isinstance(receiver, int) and isinstance(argument, int):
                    res = ovfcheck(op(intmask(receiver), intmask(argument)))
                    return interp.space.wrap_int(intmask(res))
                else:
                    raise PrimitiveFailedError()
            except OverflowError:
                raise PrimitiveFailedError()
        func.func_name = name
        LargeIntegerPlugin.expose_primitive(unwrap_specs=[[int, int], [pos_32bit_int, pos_32bit_int], [r_uint, r_uint]])(func)
    make_func(op)
