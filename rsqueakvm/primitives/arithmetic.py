import math
import operator

from rsqueakvm import constants, wrapper
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.numeric import W_Float
from rsqueakvm.primitives import expose_primitive, expose_also_as, positive_uint
from rsqueakvm.primitives.constants import *

from rpython.rlib import rfloat, jit
from rpython.rlib.rbigint import rbigint, NULLRBIGINT, ONERBIGINT
from rpython.rlib.rarithmetic import intmask, r_uint, ovfcheck, ovfcheck_float_to_int, r_int64, is_valid_int
from rpython.rlib.objectmodel import specialize

combination_specs = [[int, int], [r_int64, r_int64], [rbigint, rbigint]]
comparison_specs = combination_specs + [[float, float]]
arithmetic_specs = comparison_specs


def make_ovfcheck(op, check):
    @specialize.argtype(0, 1)
    def fun(x, y):
        if isinstance(x, float) and isinstance(y, float):
            return op(x, y)
        elif not (isinstance(x, int) and isinstance(y, int)):
            try: r = op(x, y)
            except OverflowError: raise PrimitiveFailedError # not raised after translation :(
            if check(x, y, r): raise PrimitiveFailedError
            return r
        else:
            try: return ovfcheck(op(x, y))
            except OverflowError: raise PrimitiveFailedError
    return fun
# div overflows only in one case
int_ovfcheck_div = make_ovfcheck(operator.floordiv, lambda x,y,r: (x == constants.MININT) & (y == -1))
# mod overflows like div
int_ovfcheck_mod = make_ovfcheck(operator.mod, lambda x,y,r: (x == constants.MININT) & (y == -1))
# add overflows if the result has a different sign than both operands
int_ovfcheck_add = make_ovfcheck(operator.add, lambda x,y,r: (((r^x)<0) & ((r^y)<0)))
# sub overflows if the result has a different sign than x and negated y
int_ovfcheck_sub = make_ovfcheck(operator.sub, lambda x,y,r: (((r^x)<0) & ((r^~y)<0)))
# mul overflow check uses doubles to do a conservative overflow check
int_ovfcheck_mul = make_ovfcheck(operator.mul, lambda x,y,r: float(x)*float(y) != float(r))


# ___________________________________________________________________________
# Boolean Primitives

bool_ops = {
    LESSTHAN: (operator.lt, rbigint.lt),
    GREATERTHAN: (operator.gt, rbigint.gt),
    LESSOREQUAL: (operator.le, rbigint.le),
    GREATEROREQUAL: (operator.ge, rbigint.ge),
    EQUAL: (operator.eq, rbigint.eq),
    NOTEQUAL: (operator.ne, rbigint.ne),
    }
for (code, ops) in bool_ops.items():
    def make_func(ops):
        smallop = ops[0]
        bigop = ops[1]
        @expose_also_as(code + LARGE_OFFSET)
        @expose_primitive(code, unwrap_specs=comparison_specs)
        def func(interp, s_frame, v1, v2):
            if not (isinstance(v1, rbigint) and isinstance(v2, rbigint)):
                res = smallop(v1, v2)
            else:
                res = bigop(v1, v2)
            return interp.space.wrap_bool(res)
    make_func(ops)
else:
    del code, ops

for (code, ops) in bool_ops.items():
    def make_func(ops):
        smallop = ops[0]
        @expose_primitive(code + FLOAT_OFFSET, unwrap_spec=[float, float])
        def func(interp, s_frame, v1, v2):
            res = smallop(v1, v2)
            w_res = interp.space.wrap_bool(res)
            return w_res
    make_func(ops)
else:
    del code, ops

# ___________________________________________________________________________
# SmallInteger Primitives

math_ops = {
    ADD: (int_ovfcheck_add, rbigint.add),
    SUBTRACT: (int_ovfcheck_sub, rbigint.sub),
    MULTIPLY: (int_ovfcheck_mul, rbigint.mul),
    }
for (code, ops) in math_ops.items():
    def make_func(ops, code):
        smallop = ops[0]
        bigop = ops[1]
        @expose_also_as(code + LARGE_OFFSET)
        @expose_primitive(code, unwrap_specs=arithmetic_specs)
        def func(interp, s_frame, receiver, argument):
            if isinstance(receiver, rbigint) and isinstance(argument, rbigint):
                return interp.space.wrap_rbigint(bigop(receiver, argument))
            elif isinstance(receiver, float) and isinstance(argument, float):
                return interp.space.wrap_float(smallop(receiver, argument))
            else:
                return interp.space.wrap_int(smallop(receiver, argument))
    make_func(ops, code)
else:
    del code, ops

bitwise_binary_ops = {
    BIT_AND: (operator.and_, rbigint.and_),
    BIT_OR: (operator.or_, rbigint.or_),
    BIT_XOR: (operator.xor, rbigint.xor),
    }
for (code, ops) in bitwise_binary_ops.items():
    def make_func(smallop, bigop):
        @expose_also_as(code + LARGE_OFFSET)
        @expose_primitive(code, unwrap_specs=[[int, int], [positive_uint, positive_uint], [rbigint, rbigint]])
        def func(interp, s_frame, receiver, argument):
            if isinstance(receiver, rbigint):
                return interp.space.wrap_int(bigop(receiver, argument))
            else:
                return interp.space.wrap_int(smallop(receiver, argument))
    make_func(ops[0], ops[1])
else:
    del code, ops

@specialize.argtype(0)
def guard_nonnull(value):
    if not isinstance(value, rbigint) and value == 0:
        raise PrimitiveFailedError
    if isinstance(value, rbigint) and value == NULLRBIGINT:
        raise PrimitiveFailedError

# #/ -- return the result of a division, only succeed if the division is exact
@expose_also_as(LARGE_DIVIDE)
@expose_primitive(DIVIDE, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    guard_nonnull(argument)
    if isinstance(receiver, rbigint):
        if receiver.mod(argument) != NULLRBIGINT:
            raise PrimitiveFailedError
        return interp.space.wrap_rbigint(receiver.div(argument))
    else:
        if int_ovfcheck_mod(receiver, argument) != 0:
            raise PrimitiveFailedError()
        return interp.space.wrap_int(int_ovfcheck_div(receiver, argument))

# #\\ -- return the remainder of a division
@expose_also_as(LARGE_MOD)
@expose_primitive(MOD, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    guard_nonnull(argument)
    if isinstance(receiver, rbigint):
        return interp.space.wrap_rbigint(receiver.mod(argument))
    else:
        return interp.space.wrap_int(int_ovfcheck_mod(receiver, argument))

# #// -- return the result of a division, rounded towards negative infinity
@expose_also_as(LARGE_DIV)
@expose_primitive(DIV, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    guard_nonnull(argument)
    if isinstance(receiver, rbigint):
        return interp.space.wrap_rbigint(receiver.div(argument))
    else:
        return interp.space.wrap_int(int_ovfcheck_div(receiver, argument))

# #// -- return the result of a division, rounded towards negative infinite
@expose_also_as(LARGE_QUO)
@expose_primitive(QUO, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    guard_nonnull(argument)
    # see http://python-history.blogspot.de/2010/08/why-pythons-integer-division-floors.html
    if isinstance(receiver, rbigint):
        res = receiver.div(argument)
        if res.lt(NULLRBIGINT) and not receiver.abs() == argument.abs():
            res = res.add(ONERBIGINT)
        return interp.space.wrap_rbigint(res)
    else:
        res = int_ovfcheck_div(receiver, argument)
        if res < 0 and not abs(receiver) == abs(argument):
            res = res + 1
        return interp.space.wrap_int(res)

# #bitShift: -- return the shifted value
@expose_also_as(LARGE_BIT_SHIFT)
@expose_primitive(BIT_SHIFT, unwrap_specs=[[int, int], [rbigint, int]])
def func(interp, s_frame, receiver, shift):
    if shift > 0:
        if isinstance(receiver, int):
            try:
                if receiver >= 0:
                    return interp.space.wrap_int(r_uint(ovfcheck(receiver << shift)))
                else:
                    return interp.space.wrap_int((ovfcheck(receiver << shift)))
            except OverflowError:
                raise PrimitiveFailedError
        else:
            return interp.space.wrap_rbigint(receiver.lshift(shift))
    elif shift == 0:
        return interp.space.wrap_int(receiver)
    else:
        shift = -shift
        assert shift >= 0
        if isinstance(receiver, int):
            return interp.space.wrap_int(receiver >> shift)
        else:
            return interp.space.wrap_rbigint(receiver.rshift(shift))

# ___________________________________________________________________________
# Float Primitives

@expose_primitive(SMALLINT_AS_FLOAT, unwrap_spec=[int])
def func(interp, s_frame, i):
    return interp.space.wrap_float(float(i))

math_ops = {
    FLOAT_ADD: operator.add,
    FLOAT_SUBTRACT: operator.sub,
    FLOAT_MULTIPLY: operator.mul,
    }
for (code, op) in math_ops.items():
    def make_func(op):
        @expose_primitive(code, unwrap_spec=[float, float])
        def func(interp, s_frame, v1, v2):
            w_res = interp.space.wrap_float(op(v1, v2))
            return w_res
    make_func(op)
else:
    del code, op

@expose_primitive(FLOAT_DIVIDE, unwrap_spec=[float, float])
def func(interp, s_frame, v1, v2):
    if v2 == 0:
        raise PrimitiveFailedError
    return interp.space.wrap_float(v1 / v2)

@expose_primitive(FLOAT_TRUNCATED, unwrap_spec=[float])
def func(interp, s_frame, f):
    try:
        return interp.space.wrap_int(ovfcheck_float_to_int(f))
    except OverflowError:
        raise PrimitiveFailedError

@expose_primitive(FLOAT_TIMES_TWO_POWER, unwrap_spec=[float, int])
def func(interp, s_frame, rcvr, arg):
    from rpython.rlib.rfloat import INFINITY
    # http://www.python.org/dev/peps/pep-0754/
    try:
        return interp.space.wrap_float(math.ldexp(rcvr, arg))
    except OverflowError:
        if rcvr >= 0.0:
            return W_Float(INFINITY)
        else:
            return W_Float(-INFINITY)

@expose_primitive(FLOAT_SQUARE_ROOT, unwrap_spec=[float])
def func(interp, s_frame, f):
    if f < 0.0:
        raise PrimitiveFailedError
    w_res = interp.space.wrap_float(math.sqrt(f))
    return w_res

@expose_primitive(FLOAT_SIN, unwrap_spec=[float])
def func(interp, s_frame, f):
    try:
        return interp.space.wrap_float(math.sin(f))
    except ValueError:
        return interp.space.wrap_float(rfloat.NAN)

@expose_primitive(FLOAT_ARCTAN, unwrap_spec=[float])
def func(interp, s_frame, f):
    w_res = interp.space.wrap_float(math.atan(f))
    return w_res

@expose_primitive(FLOAT_LOG_N, unwrap_spec=[float])
def func(interp, s_frame, f):
    if f == 0:
        res = -rfloat.INFINITY
    elif f < 0:
        res = rfloat.NAN
    else:
        res = math.log(f)
    return interp.space.wrap_float(res)

@expose_primitive(FLOAT_EXP, unwrap_spec=[float])
def func(interp, s_frame, f):
    try:
        return interp.space.wrap_float(math.exp(f))
    except OverflowError:
        return interp.space.wrap_float(rfloat.INFINITY)


@expose_primitive(MAKE_POINT, unwrap_spec=[int, int])
def func(interp, s_frame, x, y):
    w_res = interp.space.w_Point.as_class_get_shadow(interp.space).new()
    point = wrapper.PointWrapper(interp.space, w_res)
    point.store_x(x)
    point.store_y(y)
    return w_res

# ____________________________________________________________________________
# Time Primitives (135 - 137, 240 - 242)

@jit.elidable
def event_time_to_microseconds(interp, ev_time):
    """
    The microsecond-based time primitives are relative to a roll-over (we use
    startup time). The millisecond-based ones are based on the Squeak-Epoch
    (1901).

    This function converts the former to the latter by: scaling up, adding
    image startup timestamp, and finally adding the Epoch constant.
    """
    secs_to_usecs = 1000 * 1000
    return r_int64(ev_time * 1000 + interp.startup_time * secs_to_usecs) + \
        constants.SQUEAK_EPOCH_DELTA_MICROSECONDS

@expose_primitive(MILLISECOND_CLOCK, unwrap_spec=[object])
def func(interp, s_frame, w_arg):
    x = interp.event_time_now()
    return interp.space.wrap_int(x)

@expose_primitive(SIGNAL_AT_MILLISECONDS, unwrap_spec=[object, object, int])
def func(interp, s_frame, w_delay, w_semaphore, ev_timestamp):
    if not w_semaphore.getclass(interp.space).is_same_object(
            interp.space.w_Semaphore):
        interp.space.objtable["w_timerSemaphore"] = interp.space.w_nil
    else:
        interp.space.objtable["w_timerSemaphore"] = w_semaphore
    interp.next_wakeup_tick = event_time_to_microseconds(interp, ev_timestamp)
    return w_delay


@expose_primitive(SECONDS_CLOCK, unwrap_spec=[object])
def func(interp, s_frame, w_arg):
    secs_since_1901 = r_uint(interp.time_now() / 1000000)
    return interp.space.wrap_int(secs_since_1901)

@expose_primitive(UTC_MICROSECOND_CLOCK, unwrap_spec=[object])
def func(interp, s_frame, w_arg):
    return interp.space.wrap_int(interp.time_now())

@expose_primitive(LOCAL_MICROSECOND_CLOCK, unwrap_spec=[object])
def func(interp, s_frame, w_arg):
    # XXX: For now, pretend we are UTC. More later...
    x = interp.time_now()
    return interp.space.wrap_int(x)

@expose_primitive(SIGNAL_AT_UTC_MICROSECONDS,
                  unwrap_spec=[object, object, r_int64])
def func(interp, s_frame, w_delay, w_semaphore, timestamp):
    if not w_semaphore.getclass(interp.space).is_same_object(
            interp.space.w_Semaphore):
        interp.space.objtable["w_timerSemaphore"] = interp.space.w_nil
    else:
        interp.space.objtable["w_timerSemaphore"] = w_semaphore
    interp.next_wakeup_tick = timestamp
    return w_delay
