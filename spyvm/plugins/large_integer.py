from spyvm import model
from spyvm.error import PrimitiveFailedError
from spyvm.plugins.plugin import Plugin

from rpython.rlib.rbigint import rbigint


LargeIntegerPlugin = Plugin()


ops = {
    'primDigitBitAnd': (rbigint.and_, False),
    'primDigitBitOr': (rbigint.or_, False),
    'primDigitBitXor': (rbigint.xor, False),
    'primDigitAdd': (rbigint.add, True),
    'primDigitSubtract': (rbigint.sub, True)
}
for (name, (op, absolute)) in ops.items():
    def make_func(op, absolute):
        def func(interp, s_frame, a, b):
            if absolute:
                a = a.abs()
                b = b.abs()
            return interp.space.wrap_bigint(op(a, b))
        func.func_name = name
        LargeIntegerPlugin.expose_primitive(unwrap_spec=[rbigint, rbigint])(func)
    make_func(op, absolute)


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
