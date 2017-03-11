from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives import prim_table
from rsqueakvm.primitives.constants import *
from rsqueakvm.model.numeric import (
    W_LargeIntegerBig, calculate_exposed_size_for_big_int)
from rsqueakvm.model.variable import W_BytesObject

from rpython.rlib.rbigint import rbigint, _divrem
from rpython.rlib import jit


class LargeIntegers(Plugin):
    pass

plugin = LargeIntegers()

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
        plugin.expose_primitive(clean_stack=False, no_result=True)(func)
    make_func(name, primitive)


@plugin.expose_primitive(unwrap_spec=[rbigint, rbigint])
def primDigitAdd(interp, s_frame, rcvr, arg):
    return interp.space.wrap_rbigint(rcvr.add(arg))


@plugin.expose_primitive(unwrap_spec=[rbigint, rbigint])
def primDigitSubtract(interp, s_frame, rcvr, arg):
    return interp.space.wrap_rbigint(rcvr.sub(arg))


@plugin.expose_primitive(unwrap_spec=[rbigint, rbigint, object])
def primDigitMultiplyNegative(interp, s_frame, rcvr, arg, neg):
    return interp.space.wrap_rbigint(rcvr.mul(arg))


@plugin.expose_primitive(unwrap_spec=[rbigint, rbigint, object])
def primDigitDivNegative(interp, s_frame, rcvr, arg, neg):
    try:
        quo, rem = _divrem(rcvr, arg)
    except ZeroDivisionError:
        raise PrimitiveFailedError
    return interp.space.wrap_list_unroll_safe([
        interp.space.wrap_rbigint(quo),
        interp.space.wrap_rbigint(rem)
    ])


@plugin.expose_primitive(unwrap_specs=[[int, int], [object, int],
                         [rbigint, rbigint]])
def primDigitCompare(interp, s_frame, rcvr, arg):
    # the C code is different than the fallback!! The C code does a normal
    # compare for small integers, and if the first isn't but the second is an
    # integer, it returns 1!!
    if isinstance(rcvr, int):
        if rcvr > arg:
            res = 1
        elif rcvr < arg:
            res = -1
        else:
            res = 0
    elif isinstance(arg, int):
        res = 1
    else:
        if rcvr.sign != arg.sign:
            rcvr = rcvr.abs()
            arg = arg.abs()
        if rcvr.gt(arg):
            res = 1
        elif rcvr.lt(arg):
            res = -1
        else:
            res = 0
    return interp.space.wrap_int(res)


@jit.elidable
def minimum_bytelen_for(val):
    bytelen = calculate_exposed_size_for_big_int(val) - 1
    while True:
        try:
            # see if we have enough room
            val.tobytes(bytelen, 'little', False)
            return bytelen
        except OverflowError:
            bytelen += 1


@plugin.expose_primitive(unwrap_spec=[object])
def primNormalizePositive(interp, s_frame, w_rcvr):
    if isinstance(w_rcvr, W_BytesObject):
        # only bytes object may be denormalized
        return interp.space.wrap_rbigint(w_rcvr.unwrap_rbigint(interp.space))
    elif isinstance(w_rcvr, W_LargeIntegerBig):
        w_rcvr._exposed_size = minimum_bytelen_for(w_rcvr.value)
    return w_rcvr


@plugin.expose_primitive(unwrap_spec=[object])
def primNormalizeNegative(interp, s_frame, w_rcvr):
    if isinstance(w_rcvr, W_BytesObject):
        # only bytes object may be denormalized
        return interp.space.wrap_rbigint(w_rcvr.unwrap_rbigint(interp.space))
    elif isinstance(w_rcvr, W_LargeIntegerBig):
        w_rcvr._exposed_size = minimum_bytelen_for(w_rcvr.value.abs())
    return w_rcvr
