import math
import operator

from rsqueakvm import constants, wrapper
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.numeric import W_Float
from rsqueakvm.primitives import expose_primitive, expose_also_as, pos_32bit_int
from rsqueakvm.primitives.bytecodes import *

from rpython.rlib import rfloat, jit
from rpython.rlib.rarithmetic import intmask, r_uint, ovfcheck, ovfcheck_float_to_int, r_int64
from rpython.rlib.objectmodel import specialize

combination_specs = [[int, int], [pos_32bit_int, pos_32bit_int], [r_int64, r_int64]]

# ___________________________________________________________________________
# Boolean Primitives

bool_ops = {
    LESSTHAN: operator.lt,
    GREATERTHAN: operator.gt,
    LESSOREQUAL: operator.le,
    GREATEROREQUAL: operator.ge,
    EQUAL: operator.eq,
    NOTEQUAL: operator.ne
    }
for (code, op) in bool_ops.items():
    def make_func(op):
        @expose_also_as(code + LARGE_OFFSET)
        @expose_primitive(code, unwrap_specs=combination_specs)
        def func(interp, s_frame, v1, v2):
            res = op(v1, v2)
            w_res = interp.space.wrap_bool(res)
            return w_res
    make_func(op)

for (code, op) in bool_ops.items():
    def make_func(op):
        @expose_primitive(code + FLOAT_OFFSET, unwrap_spec=[float, float])
        def func(interp, s_frame, v1, v2):
            res = op(v1, v2)
            w_res = interp.space.wrap_bool(res)
            return w_res
    make_func(op)

# ___________________________________________________________________________
# SmallInteger Primitives

@jit.dont_look_inside
@specialize.arg(1)
def overflow_math_op_64bit(interp, code, receiver, argument):
    from rpython.rlib.rarithmetic import r_longlonglong
    if code == ADD:
        res = r_longlonglong(receiver) + r_longlonglong(argument)
    elif code == SUBTRACT:
        res = r_longlonglong(receiver) - r_longlonglong(argument)
    elif code == MULTIPLY:
        res = r_longlonglong(receiver) * r_longlonglong(argument)
    else:
        assert False
    return interp.space.wrap_longlonglong(res)

math_ops = {
    ADD: operator.add,
    SUBTRACT: operator.sub,
    MULTIPLY: operator.mul,
    }
for (code, op) in math_ops.items():
    def make_func(op, code):
        @expose_also_as(code + LARGE_OFFSET)
        @expose_primitive(code, unwrap_specs=[[int, int], [r_int64, r_int64]])
        def func(interp, s_frame, receiver, argument):
            try:
                if isinstance(receiver, int) and isinstance(argument, int):
                    res = ovfcheck(op(receiver, argument))
                elif ((not constants.IS_64BIT) and
                      isinstance(receiver, r_int64) and
                      isinstance(argument, r_int64)):
                    res = op(receiver, argument)
                    if ((receiver ^ argument >= 0) and (receiver ^ res < 0)):
                        # manual ovfcheck as in Squeak VM
                        raise OverflowError
                else:
                    assert False
            except OverflowError:
                if constants.IS_64BIT:
                    return overflow_math_op_64bit(interp, code, receiver, argument)
                raise PrimitiveFailedError()
            return interp.space.wrap_int(res)
    make_func(op, code)

bitwise_binary_ops = {
    BIT_AND: operator.and_,
    BIT_OR: operator.or_,
    BIT_XOR: operator.xor,
    }
for (code, op) in bitwise_binary_ops.items():
    def make_func(op):
        @expose_also_as(code + LARGE_OFFSET)
        @expose_primitive(code, unwrap_specs=[[int, int], [r_uint, r_uint]])
        def func(interp, s_frame, receiver, argument):
            res = op(intmask(receiver), intmask(argument))
            if isinstance(receiver, r_uint):
                return interp.space.wrap_positive_wordsize_int(intmask(res))
            else:
                return interp.space.wrap_int(intmask(res))
    make_func(op)

def make_ovfcheck(op):
    @specialize.argtype(0, 1)
    def fun(receiver, argument):
        if isinstance(receiver, r_uint) and isinstance(argument, r_uint):
            return op(receiver, argument)
        elif ((not constants.IS_64BIT) and
              isinstance(receiver, r_int64) and
              isinstance(argument, r_int64)):
            res = op(receiver, argument)
            if ((receiver ^ argument >= 0) and (receiver ^ res < 0)):
                # manual ovfcheck as in Squeak VM
                raise PrimitiveFailedError
            return res
        else:
            try:
                return ovfcheck(op(receiver, argument))
            except OverflowError:
                raise PrimitiveFailedError
    return fun
ovfcheck_div = make_ovfcheck(operator.floordiv)
ovfcheck_mod = make_ovfcheck(operator.mod)

# #/ -- return the result of a division, only succeed if the division is exact
@expose_also_as(LARGE_DIVIDE)
@expose_primitive(DIVIDE, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    if argument == 0:
        raise PrimitiveFailedError()
    if ovfcheck_mod(receiver, argument) != 0:
        raise PrimitiveFailedError()
    return interp.space.wrap_int(ovfcheck_div(receiver, argument))

# #\\ -- return the remainder of a division
@expose_also_as(LARGE_MOD)
@expose_primitive(MOD, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    if argument == 0:
        raise PrimitiveFailedError()
    return interp.space.wrap_int(ovfcheck_mod(receiver, argument))

# #// -- return the result of a division, rounded towards negative infinity
@expose_also_as(LARGE_DIV)
@expose_primitive(DIV, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    if argument == 0:
        raise PrimitiveFailedError()
    return interp.space.wrap_int(ovfcheck_div(receiver, argument))

# #// -- return the result of a division, rounded towards negative infinite
@expose_also_as(LARGE_QUO)
@expose_primitive(QUO, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    if argument == 0:
        raise PrimitiveFailedError()
    res = ovfcheck_div(receiver, argument)
    # see http://python-history.blogspot.de/2010/08/why-pythons-integer-division-floors.html
    if res < 0 and not abs(receiver) == abs(argument):
        res = res + 1
    return interp.space.wrap_int(res)

# #bitShift: -- return the shifted value
@expose_also_as(LARGE_BIT_SHIFT)
@expose_primitive(BIT_SHIFT, unwrap_spec=[object, int])
def func(interp, s_frame, w_receiver, argument):
    if -constants.LONG_BIT < argument < constants.LONG_BIT:
        # overflow-checking done in lshift implementations
        if argument > 0:
            return w_receiver.lshift(interp.space, argument)
        else:
            return w_receiver.rshift(interp.space, -argument)
    else:
        raise PrimitiveFailedError()

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
    w_res = interp.space.w_Point.as_class_get_shadow(interp.space).new(2)
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
    return interp.space.wrap_uint(secs_since_1901)


@expose_primitive(UTC_MICROSECOND_CLOCK, unwrap_spec=[object])
def func(interp, s_frame, w_arg):
    return interp.space.wrap_longlong(interp.time_now())

@expose_primitive(LOCAL_MICROSECOND_CLOCK, unwrap_spec=[object])
def func(interp, s_frame, w_arg):
    # XXX: For now, pretend we are UTC. More later...
    x = interp.time_now()
    return interp.space.wrap_longlong(x)

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
