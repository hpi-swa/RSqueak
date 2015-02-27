from spyvm import model
from spyvm.primitives import BIT_AND, BIT_OR, BIT_XOR, prim_table
from spyvm.error import PrimitiveFailedError
from spyvm.plugins.plugin import Plugin

from rpython.rlib.rbigint import rbigint


LargeIntegerPlugin = Plugin()


ops = {
    'primDigitBitAnd': (rbigint.and_, BIT_AND),
    'primDigitBitOr': (rbigint.or_, BIT_OR),
    'primDigitBitXor': (rbigint.xor, BIT_XOR)
}
for (name, (op, primitive)) in ops.items():
    def make_func(op, primitive):
        primfunc = prim_table[primitive]
        def bigfunc(interp, s_frame, a, b):
            return interp.space.wrap_bigint(op(a, b))
        bigfunc.func_name = "some_unlikely_%s" % name
        LargeIntegerPlugin.expose_primitive(unwrap_spec=[rbigint, rbigint])(bigfunc)
        wrapped_bigfunc = LargeIntegerPlugin.prims[bigfunc.func_name]
        # We try to run the 32bit integer primitive first
        def func(interp, s_frame, argcount):
            try:
                primfunc(interp, s_frame, argcount)
            except PrimitiveFailedError:
                wrapped_bigfunc(interp, s_frame, argcount)
        func.func_name = name
        LargeIntegerPlugin.expose_primitive(clean_stack=False, no_result=True)(func)
    make_func(op, primitive)


arithOps = {
    'primDigitAdd': rbigint.add,
    'primDigitSubtract': rbigint.sub
}
for (name, op) in arithOps.items():
    def make_func(op):
        def func(interp, s_frame, a, b):
            return interp.space.wrap_bigint(op(a.abs(), b.abs()))
        func.func_name = name
        LargeIntegerPlugin.expose_primitive(unwrap_spec=[rbigint, rbigint])(func)
    make_func(op)


@LargeIntegerPlugin.expose_primitive(unwrap_spec=[rbigint, rbigint, bool])
def primDigitDivNegative(interp, s_frame, self, arg, neg):
    quo, rem = self.abs().divmod(arg)
    if neg: quo = quo.neg()
    return interp.space.wrap_list([
        interp.space.wrap_bigint(quo),
        interp.space.wrap_bigint(rem)
    ])


@LargeIntegerPlugin.expose_primitive(unwrap_spec=[rbigint, rbigint, bool])
def primDigitMultiplyNegative(interp, s_frame, self, arg, neg):
    res = self.abs().mul(arg)
    if neg: res.neg()
    return interp.space.wrap_bigint(res)


@LargeIntegerPlugin.expose_primitive(unwrap_spec=[rbigint, int])
def primDigitBitShiftMagnitude(interp, s_frame, a, b):
    if b >= 0:
        return interp.space.wrap_bigint(a.lshift(b))
    else:
        return interp.space.wrap_bigint(a.rshift(-b))



@LargeIntegerPlugin.expose_primitive(unwrap_spec=[rbigint, rbigint])
def primDigitCompare(interp, s_frame, a, b):
    a = a.abs()
    b = b.abs()
    if a.eq(b):
        res = 0
    elif a.gt(b):
        res = 1
    else:
        res = -1
    return interp.space.wrap_int(res)


@LargeIntegerPlugin.expose_primitive(unwrap_spec=[rbigint])
def primNormalizePositive(interp, s_frame, self):
    return interp.space.wrap_bigint(self)


@LargeIntegerPlugin.expose_primitive(unwrap_spec=[rbigint])
def primNormalizeNegative(interp, s_frame, self):
    return interp.space.wrap_bigint(self)
