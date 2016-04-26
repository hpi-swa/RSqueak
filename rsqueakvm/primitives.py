import os
import inspect
import math
import operator

from rsqueakvm import storage_contexts, error, constants, display, wrapper
from rsqueakvm.error import MetaPrimFailed, PrimitiveFailedError, PrimitiveNotYetWrittenError
from rsqueakvm.model.base import W_Object
from rsqueakvm.model.character import W_Character
from rsqueakvm.model.compiled_methods import W_CompiledMethod, W_PreSpurCompiledMethod, W_SpurCompiledMethod
from rsqueakvm.model.display import W_DisplayBitmap
from rsqueakvm.model.numeric import W_Float, W_SmallInteger, W_LargePositiveInteger1Word
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject

from rpython.rlib import rfloat, unroll, jit, objectmodel
from rpython.rlib.rarithmetic import intmask, r_uint, ovfcheck, ovfcheck_float_to_int, r_int64, int_between


def assert_class(interp, w_obj, w_class):
    if not w_obj.getclass(interp.space).is_same_object(w_class):
        raise PrimitiveFailedError()

def assert_valid_index(space, n0, w_obj):
    if not int_between(0, n0, w_obj.varsize()):
        raise PrimitiveFailedError()
    # return the index, since from here on the annotator knows that
    # n0 cannot be negative
    return n0

def assert_pointers(w_obj):
    if not (isinstance(w_obj, W_PointersObject) or isinstance(w_obj, W_Character)):
        raise PrimitiveFailedError
    return w_obj

# indicates that what is pushed is an index1, but it is unwrapped and
# converted to an index0
index1_0 = object()
char = object()
pos_32bit_int = object()


def unwrap_alternatives(unwrap_specs=None):
    assert unwrap_specs
    length = len(unwrap_specs[0])
    for spec in unwrap_specs:
        assert length == len(spec)
    positions = range(2, length+2)[:]
    from rpython.rlib.unroll import unrolling_iterable
    def decorator(func):
        func = objectmodel.specialize.argtype(*positions)(func)
        functions = []
        for spec in unwrap_specs:
            primfunc = wrap_primitive(unwrap_spec=spec)(func)
            if not objectmodel.we_are_translated():
                primfunc.func_name = "%s_%s" % (primfunc.func_name, str(spec))
            functions.append(primfunc)
        unrolling_funcs = unrolling_iterable(functions)
        def wrapped(interp, s_frame, argument_count, w_method=None):
            for func in unrolling_funcs:
                try:
                    return func(interp, s_frame, argument_count, w_method=w_method)
                except PrimitiveFailedError:
                    pass
            raise PrimitiveFailedError
        wrapped.func_name = "wrapped_alternatives_%s" % func.func_name
        return wrapped
    return decorator

def wrap_primitive(unwrap_spec=None, no_result=False,
                   result_is_new_frame=False, may_context_switch=True,
                   clean_stack=True, compiled_method=False):
    # some serious magic, don't look
    from rpython.rlib.unroll import unrolling_iterable

    assert not (no_result and result_is_new_frame)
    assert may_context_switch or result_is_new_frame

    # Because methods always have a receiver, an unwrap_spec of [] is a bug
    assert unwrap_spec is None or unwrap_spec

    def decorator(func):
        if unwrap_spec is None:
            def wrapped(interp, s_frame, argument_count_m1, w_method=None):
                if compiled_method:
                    result = func(interp, s_frame, argument_count_m1, w_method)
                else:
                    result = func(interp, s_frame, argument_count_m1)
                if result_is_new_frame:
                    return interp.stack_frame(result, s_frame, may_context_switch)
                if not no_result:
                    assert result is not None
                    s_frame.push(result)
        else:
            len_unwrap_spec = len(unwrap_spec)
            assert len_unwrap_spec + 2 == len(inspect.getargspec(func)[0]), "wrong number of arguments"
            unrolling_unwrap_spec = unrolling_iterable(enumerate(unwrap_spec))
            def wrapped(interp, s_frame, argument_count_m1, w_method=None):
                argument_count = argument_count_m1 + 1 # to account for the rcvr
                assert argument_count == len_unwrap_spec
                if s_frame.stackdepth() < len_unwrap_spec:
                    raise PrimitiveFailedError()
                args = ()
                for i, spec in unrolling_unwrap_spec:
                    index = len_unwrap_spec - 1 - i
                    w_arg = s_frame.peek(index)
                    if spec is int:
                        args += (interp.space.unwrap_int(w_arg), )
                    elif spec is pos_32bit_int:
                        args += (interp.space.unwrap_positive_wordsize_int(w_arg),)
                    elif spec is r_uint:
                        args += (interp.space.unwrap_uint(w_arg),)
                    elif spec is r_int64:
                        args += (interp.space.unwrap_longlong(w_arg),)
                    elif spec is index1_0:
                        args += (interp.space.unwrap_int(w_arg)-1, )
                    elif spec is float:
                        args += (interp.space.unwrap_float(w_arg), )
                    elif spec is object:
                        assert isinstance(w_arg, W_Object)
                        args += (w_arg, )
                    elif spec is str:
                        assert isinstance(w_arg, W_BytesObject)
                        args += (interp.space.unwrap_string(w_arg), )
                    elif spec is list:
                        assert isinstance(w_arg, W_PointersObject)
                        args += (interp.space.unwrap_array(w_arg), )
                    elif spec is char:
                        args += (interp.space.unwrap_char_as_byte(w_arg), )
                    elif spec is bool:
                        args += (interp.space.w_true is w_arg, )
                    else:
                        raise NotImplementedError(
                            "unknown unwrap_spec %s" % (spec, ))
                if result_is_new_frame:
                    s_new_frame = func(interp, s_frame, *args)
                    # After calling primitive, reload context-shadow in case it
                    # needs to be updated
                    if clean_stack:
                        # happens only if no exception occurs!
                        s_frame.pop_n(len_unwrap_spec)
                    return interp.stack_frame(s_new_frame, s_frame, may_context_switch)
                else:
                    w_result = func(interp, s_frame, *args)
                    # After calling primitive, reload context-shadow in case it
                    # needs to be updated
                    if clean_stack:
                        # happens only if no exception occurs!
                        s_frame.pop_n(len_unwrap_spec)
                    if not no_result:
                        assert w_result is not None
                        assert isinstance(w_result, W_Object)
                        s_frame.push(w_result)
        wrapped.func_name = "wrapped_%s" % func.func_name
        return wrapped
    return decorator

def expose_primitive(code, wrap_func=None, **kwargs):
    # heuristics to give it a nice name
    name = None
    for key, value in globals().iteritems():
        if isinstance(value, int) and value == code and key == key.upper():
            if name is not None:
                # refusing to guess
                name = "unknown"
            else:
                name = key
    if not wrap_func:
        if kwargs.get('unwrap_specs', None):
            wrap_func = unwrap_alternatives
        else:
            wrap_func = wrap_primitive
    def decorator(func):
        assert code not in prim_table
        func.func_name = "prim_" + name
        wrapped = wrap_func(**kwargs)(func)
        wrapped.func_name = "wrap_prim_" + name
        prim_table[code] = wrapped
        prim_table_implemented_only.append((code, wrapped))
        return func
    return decorator

def expose_also_as(*codes):
    def decorator(func):
        wrapped = prim_table[globals()[func.func_name.replace('prim_', '')]]
        for code in codes:
            assert code not in prim_table
            prim_table[code] = wrapped
            prim_table_implemented_only.append((code, wrapped))
        return wrapped
    return decorator

# ___________________________________________________________________________
# Primitive table: it is filled in at initialization time with the
# primitive functions.  Each primitive function takes two
# arguments, an interp and an argument_count
# completes, and returns a result, or throws a PrimitiveFailedError.
def make_simulation(code):
    p_code = jit.promote(code)
    @wrap_primitive(clean_stack=False, no_result=True, compiled_method=True)
    def try_simulation(interp, s_frame, argument_count, w_method=None):
        if interp.space.simulate_numeric_primitives.is_set():
            from rsqueakvm.plugins.simulation import SimulationPlugin
            return SimulationPlugin.simulateNumeric(p_code, interp, s_frame, argument_count, w_method)
        else:
            raise PrimitiveFailedError
    return try_simulation

# Squeak has primitives all the way up to 575
# So all optional primitives will default to the bytecode implementation
prim_table = [make_simulation(i) for i in range(576)]

class PrimitiveHolder(object):
    _immutable_fields_ = ["prim_table[*]"]

prim_holder  = PrimitiveHolder()
prim_holder.prim_table = prim_table
# clean up namespace:
del i
prim_table_implemented_only = []

# ___________________________________________________________________________
# SmallInteger Primitives


ADD         = 1
SUBTRACT    = 2
MULTIPLY    = 9
DIVIDE      = 10
MOD         = 11
DIV         = 12
QUO         = 13
BIT_AND     = 14
BIT_OR      = 15
BIT_XOR     = 16
BIT_SHIFT   = 17

_LARGE_OFFSET = 20
LARGE_REM = 20
LARGE_ADD = 21
LARGE_SUBTRACT = 22
LARGE_MULTIPLY = 29
LARGE_DIVIDE = 30
LARGE_MOD = 31
LARGE_DIV = 32
LARGE_QUO = 33
LARGE_BIT_AND = 34
LARGE_BIT_OR = 35
LARGE_BIT_XOR = 36
LARGE_BIT_SHIFT = 37

math_ops = {
    ADD: operator.add,
    SUBTRACT: operator.sub,
    MULTIPLY: operator.mul,
    }
for (code,op) in math_ops.items():
    def make_func(op):
        @expose_also_as(code + _LARGE_OFFSET)
        @expose_primitive(code, unwrap_specs=[[int, int], [r_int64, r_int64]])
        def func(interp, s_frame, receiver, argument):
            try:
                if isinstance(receiver, int) and isinstance(argument, int):
                    res = ovfcheck(op(receiver, argument))
                elif isinstance(receiver, r_int64) and isinstance(argument, r_int64):
                    res = op(receiver, argument)
                    if ((receiver ^ argument >= 0) and (receiver ^ res < 0)):
                        # manual ovfcheck as in Squeak VM
                        raise OverflowError
                else:
                    assert False
            except OverflowError:
                raise PrimitiveFailedError()
            return interp.space.wrap_int(res)
    make_func(op)

bitwise_binary_ops = {
    BIT_AND: operator.and_,
    BIT_OR: operator.or_,
    BIT_XOR: operator.xor,
    }
for (code,op) in bitwise_binary_ops.items():
    def make_func(op):
        @expose_also_as(code + _LARGE_OFFSET)
        @expose_primitive(code, unwrap_specs=[[int,int], [r_uint, r_uint]])
        def func(interp, s_frame, receiver, argument):
            res = op(intmask(receiver), intmask(argument))
            if isinstance(receiver, r_uint):
                return interp.space.wrap_positive_wordsize_int(intmask(res))
            else:
                return interp.space.wrap_int(intmask(res))
    make_func(op)

combination_specs = [[int, int], [pos_32bit_int, pos_32bit_int], [r_int64, r_int64]]
# #/ -- return the result of a division, only succeed if the division is exact
@expose_also_as(LARGE_DIVIDE)
@expose_primitive(DIVIDE, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    if argument == 0:
        raise PrimitiveFailedError()
    if receiver % argument != 0:
        raise PrimitiveFailedError()
    return interp.space.wrap_int(receiver // argument)

# #\\ -- return the remainder of a division
@expose_also_as(LARGE_MOD)
@expose_primitive(MOD, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    if argument == 0:
        raise PrimitiveFailedError()
    return interp.space.wrap_int(receiver % argument)

# #// -- return the result of a division, rounded towards negative infinity
@expose_also_as(LARGE_DIV)
@expose_primitive(DIV, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    if argument == 0:
        raise PrimitiveFailedError()
    return interp.space.wrap_int(receiver // argument)

# #// -- return the result of a division, rounded towards negative infinite
@expose_also_as(LARGE_QUO)
@expose_primitive(QUO, unwrap_specs=combination_specs)
def func(interp, s_frame, receiver, argument):
    if argument == 0:
        raise PrimitiveFailedError()
    res = receiver // argument
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

_FLOAT_OFFSET = 40
SMALLINT_AS_FLOAT = 40
FLOAT_ADD = 41
FLOAT_SUBTRACT = 42
# NB: 43 ... 48 are implemented above
FLOAT_MULTIPLY = 49
FLOAT_DIVIDE = 50
FLOAT_TRUNCATED = 51
# OPTIONAL: 52, 53
FLOAT_TIMES_TWO_POWER = 54
FLOAT_SQUARE_ROOT = 55
FLOAT_SIN = 56
FLOAT_ARCTAN = 57
FLOAT_LOG_N = 58
FLOAT_EXP = 59

@expose_primitive(SMALLINT_AS_FLOAT, unwrap_spec=[int])
def func(interp, s_frame, i):
    return interp.space.wrap_float(float(i))

math_ops = {
    FLOAT_ADD: operator.add,
    FLOAT_SUBTRACT: operator.sub,
    FLOAT_MULTIPLY: operator.mul,
    FLOAT_DIVIDE: operator.div,
    }
for (code,op) in math_ops.items():
    def make_func(op):
        @expose_primitive(code, unwrap_spec=[float, float])
        def func(interp, s_frame, v1, v2):
            w_res = interp.space.wrap_float(op(v1, v2))
            return w_res
    make_func(op)

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

MAKE_POINT = 18

@expose_primitive(MAKE_POINT, unwrap_spec=[int, int])
def func(interp, s_frame, x, y):
    w_res = interp.space.w_Point.as_class_get_shadow(interp.space).new(2)
    point = wrapper.PointWrapper(interp.space, w_res)
    point.store_x(x)
    point.store_y(y)
    return w_res


# ___________________________________________________________________________
# Failure

FAIL = 19

def get_string(w_obj):
    if isinstance(w_obj, W_BytesObject):
        return w_obj.unwrap_string(None)
    return w_obj.as_repr_string()

def exitFromHeadlessExecution(s_frame, selector="", w_message=None):
    if not objectmodel.we_are_translated():
        if getattr(s_frame.space, "testing", False):
            return # During Testing
        import pdb; pdb.set_trace()
    print "== Receiver: %s" % s_frame.w_receiver().as_repr_string()
    if isinstance(w_message, W_PointersObject):
        fields = w_message.fetch_all(s_frame.space)
        if len(fields) >= 1:
            print "== Selector: %s" % get_string(fields[0])
        if len(fields) >= 2:
            w_args = fields[1]
            if isinstance(w_args, W_PointersObject):
                arg_strings = [ get_string(w_arg) for w_arg in w_args.fetch_all(s_frame.space) ]
                if len(arg_strings) > 0:
                    print "== Arguments: %s" % ', '.join(arg_strings)
    print "== Smalltalk Stack:%s" % s_frame.print_stack()
    if selector == "":
        selector = s_frame.w_method().lookup_selector
    raise error.Exit("Unhandled %s in headless mode." % selector)

@expose_primitive(FAIL, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if interp.space.headless.is_set():
        w_message = None
        if s_frame.w_method().lookup_selector == 'doesNotUnderstand:':
            w_arguments = s_frame.w_arguments()
            if len(w_arguments) >= 1:
                w_message = w_arguments[0]
        exitFromHeadlessExecution(s_frame, w_message=w_message)
    raise PrimitiveFailedError()

# ___________________________________________________________________________
# Subscript and Stream Primitives

AT = 60
AT_PUT = 61
SIZE = 62
STRING_AT = 63
STRING_AT_PUT = 64

@expose_primitive(AT, unwrap_spec=[object, index1_0])
def func(interp, s_frame, w_obj, n0):
    n0 = assert_valid_index(interp.space, n0, w_obj)
    return w_obj.at0(interp.space, n0)

@expose_primitive(AT_PUT, unwrap_spec=[object, index1_0, object])
def func(interp, s_frame, w_obj, n0, w_val):
    n0 = assert_valid_index(interp.space, n0, w_obj)
    w_obj.atput0(interp.space, n0, w_val)
    return w_val

@expose_primitive(SIZE, unwrap_spec=[object])
def func(interp, s_frame, w_obj):
    if not w_obj.class_shadow(interp.space).isvariable():
        raise PrimitiveFailedError()
    return interp.space.wrap_int(w_obj.varsize())

@expose_primitive(STRING_AT, unwrap_spec=[object, index1_0])
def func(interp, s_frame, w_obj, n0):
    n0 = assert_valid_index(interp.space, n0, w_obj)
    # TODO: This can actually be called on any indexable object...
    if not (isinstance(w_obj, W_BytesObject) or
            isinstance(w_obj, W_WordsObject)):
        raise PrimitiveFailedError
    return interp.space.wrap_char(w_obj.getchar(n0))

@expose_primitive(STRING_AT_PUT, unwrap_spec=[object, index1_0, object])
def func(interp, s_frame, w_obj, n0, w_val):
    val = interp.space.unwrap_char_as_byte(w_val)
    n0 = assert_valid_index(interp.space, n0, w_obj)
    if not (isinstance(w_obj, W_CompiledMethod) or
            isinstance(w_obj, W_BytesObject) or
            isinstance(w_obj, W_WordsObject)):
        raise PrimitiveFailedError()
    w_obj.setchar(n0, val)
    return w_val

# ___________________________________________________________________________
# Stream Primitives

NEXT = 65
NEXT_PUT = 66
AT_END = 67


# Interlude:
# ___________________________________________________________________________
# SPUR primitives

CHARACTER_VALUE = 170
IMMEDIATE_IDENTITY_HASH = 171
SLOT_AT = 173
SLOT_AT_PUT = 174
CLASS_IDENTITY_HASH = 175
MAX_IDENTITY_HASH = 176
ALL_INSTANCES = 177
ALL_OBJECTS = 178

_maximum_identity_hash = 2**22 - 1
@expose_primitive(MAX_IDENTITY_HASH, unwrap_spec=[object])
def func(interp, s_frame, w_class):
    return interp.space.wrap_int(_maximum_identity_hash)

@expose_primitive(ALL_INSTANCES, unwrap_spec=[object])
def func(interp, s_frame, w_class):
    match_w = get_instances_array(interp, s_frame, w_class=w_class, store=False)
    return interp.space.wrap_list(match_w)

@expose_primitive(ALL_OBJECTS, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    match_w = get_instances_array(interp, s_frame, w_class=None, store=False)
    return interp.space.wrap_list(match_w)

# ___________________________________________________________________________
# Storage Management Primitives

OBJECT_AT = 68
OBJECT_AT_PUT = 69
NEW = 70
NEW_WITH_ARG = 71
ARRAY_BECOME_ONE_WAY = 72     # Blue Book: primitiveBecome
INST_VAR_AT = 73
INST_VAR_AT_PUT = 74
AS_OOP = 75
STORE_STACKP = 76             # Blue Book: primitiveAsObject
SOME_INSTANCE = 77
NEXT_INSTANCE = 78
NEW_METHOD = 79

## these primitives are also called as functions from elsewhere
## hence they have proper names

@expose_primitive(SLOT_AT, unwrap_spec=[object, index1_0])
def primitive_fetch(interp, s_frame, w_rcvr, n0):
    try:
        return w_rcvr.fetch(interp.space, n0)
    except IndexError:
        raise PrimitiveFailedError

@expose_primitive(SLOT_AT_PUT, unwrap_spec=[object, index1_0, object])
def primitive_store(interp, s_frame, w_rcvr, n0, w_value):
    try:
        w_rcvr.store(interp.space, n0, w_value)
        return w_value
    except IndexError:
        raise PrimitiveFailedError

@expose_primitive(OBJECT_AT, unwrap_spec=[object, index1_0])
def func(interp, s_frame, w_rcvr, n0):
    if not isinstance(w_rcvr, W_CompiledMethod):
        raise PrimitiveFailedError()
    return w_rcvr.literalat0(interp.space, n0)

@expose_primitive(OBJECT_AT_PUT, unwrap_spec=[object, index1_0, object])
def func(interp, s_frame, w_rcvr, n0, w_value):
    if not isinstance(w_rcvr, W_CompiledMethod):
        raise PrimitiveFailedError()
    w_rcvr.literalatput0(interp.space, n0, w_value)
    return w_value

@expose_primitive(NEW, unwrap_spec=[object])
def func(interp, s_frame, w_cls):
    w_cls = assert_pointers(w_cls)
    s_class = w_cls.as_class_get_shadow(interp.space)
    if s_class.isvariable():
        raise PrimitiveFailedError()
    return s_class.new()

@expose_primitive(NEW_WITH_ARG, unwrap_spec=[object, int])
def func(interp, s_frame, w_cls, size):
    w_cls = assert_pointers(w_cls)
    s_class = w_cls.as_class_get_shadow(interp.space)
    if not s_class.isvariable() and size != 0:
        raise PrimitiveFailedError()
    if size < 0:
        raise PrimitiveFailedError()
    try:
        return s_class.new(size)
    except MemoryError:
        raise PrimitiveFailedError

@expose_primitive(ARRAY_BECOME_ONE_WAY, unwrap_spec=[object, object])
def func(interp, s_frame, w_from, w_to):
    from_w = interp.space.unwrap_array(w_from)
    to_w = interp.space.unwrap_array(w_to)
    space = interp.space
    if len(from_w) != len(to_w):
        raise PrimitiveFailedError

    # TODO: make this fast (context-switch and stack-rebuilding?)
    s_current = s_frame
    while s_current.s_sender() is not None:
        s_current.w_self()  # just for the side effect of creating the
                            # ContextPart object
        s_current = s_current.s_sender()

    from rpython.rlib import rgc
    roots = [gcref for gcref in rgc.get_rpy_roots() if gcref]
    pending = roots[:]
    while pending:
        gcref = pending.pop()
        if not rgc.get_gcflag_extra(gcref):
            rgc.toggle_gcflag_extra(gcref)
            w_obj = rgc.try_cast_gcref_to_instance(W_Object, gcref)
            if w_obj is not None and w_obj.has_class():
                w_obj.pointers_become_one_way(space, from_w, to_w)
            pending.extend(rgc.get_rpy_referents(gcref))
    while roots:
        gcref = roots.pop()
        if rgc.get_gcflag_extra(gcref):
            rgc.toggle_gcflag_extra(gcref)
            roots.extend(rgc.get_rpy_referents(gcref))
    return w_from

@expose_primitive(INST_VAR_AT, unwrap_spec=[object, index1_0])
def func(interp, s_frame, w_rcvr, n0):
    "Fetches a fixed field from the object, and fails otherwise"
    s_class = w_rcvr.class_shadow(interp.space)
    w_cls = assert_pointers(w_rcvr)
    return primitive_fetch(interp, s_frame, w_rcvr, n0)

@expose_primitive(INST_VAR_AT_PUT, unwrap_spec=[object, index1_0, object])
def func(interp, s_frame, w_rcvr, n0, w_value):
    "Stores a value into a fixed field from the object, and fails otherwise"
    s_class = w_rcvr.class_shadow(interp.space)
    w_rcvr = assert_pointers(w_rcvr)
    return primitive_store(interp, s_frame, w_rcvr, n0, w_value)

@expose_also_as(IMMEDIATE_IDENTITY_HASH, CLASS_IDENTITY_HASH)
@expose_primitive(AS_OOP, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if isinstance(w_rcvr, W_SmallInteger):
        raise PrimitiveFailedError()
    return interp.space.wrap_int(w_rcvr.gethash())

@expose_primitive(STORE_STACKP, unwrap_spec=[object, int])
def func(interp, s_frame, w_frame, stackp):
    assert stackp >= 0
    w_frame = assert_pointers(w_frame)
    w_frame.store(interp.space, constants.CTXPART_STACKP_INDEX, interp.space.wrap_int(stackp))
    return w_frame

def get_instances_array_gc(interp, w_class=None):
    space = interp.space
    from rpython.rlib import rgc

    result_w = []
    roots = [gcref for gcref in rgc.get_rpy_roots() if gcref]
    pending = roots[:]
    while pending:
        gcref = pending.pop()
        if not rgc.get_gcflag_extra(gcref):
            rgc.toggle_gcflag_extra(gcref)
            w_obj = rgc.try_cast_gcref_to_instance(W_Object, gcref)

            if w_obj is not None and w_obj.has_class():
                w_cls = w_obj.getclass(space)
                if w_cls is not None:
                    # XXX: should not return SmallFloat64 on Spur64...
                    if ((not w_cls.is_same_object(space.w_SmallInteger)) and
                        (not (space.is_spur.is_set() and w_cls.is_same_object(space.w_Character))) and
                        (w_class is None or w_cls.is_same_object(w_class))):
                        result_w.append(w_obj)
            pending.extend(rgc.get_rpy_referents(gcref))

    rgc.clear_gcflag_extra(roots)
    rgc.assert_no_more_gcflags()
    return result_w

@jit.dont_look_inside
def get_instances_array_trace(interp, w_class, some_instance=False):
    space = interp.space
    result_w = []
    seen_w = {}
    roots = [interp.image.special_objects]
    pending = roots[:]
    while pending:
        w_obj = pending.pop()
        if w_obj and not seen_w.get(w_obj, False):
            seen_w[w_obj] = True
            if w_obj.has_class():
                w_cls = w_obj.getclass(space)
                if w_cls is not None:
                    # XXX: should not return SmallFloat64 on Spur64...
                    if ((not w_cls.is_same_object(space.w_SmallInteger)) and
                        (not (space.is_spur.is_set() and w_cls.is_same_object(space.w_Character))) and
                        (w_class is None or w_cls.is_same_object(w_class))):
                        if some_instance:
                            return [w_obj]
                        else:
                            result_w.append(w_obj)
            pending.extend(_trace_pointers(interp.space, w_obj))
    return result_w

def _trace_pointers(space, w_obj):
    p_w = [w_obj.getclass(space)]
    if isinstance(w_obj, W_CompiledMethod):
        p_w.extend(w_obj.literals)
    elif isinstance(w_obj, W_PointersObject):
        p_w.extend(w_obj.fetch_all(space))
    return p_w

def get_instances_array(interp, s_frame, w_class=None, store=True, some_instance=False):
    # make sure we also get any objects in the currently active process
    w_active_process = wrapper.scheduler(interp.space).active_process()
    active_process = wrapper.ProcessWrapper(interp.space, w_active_process)
    active_process.store_suspended_context(s_frame.w_self())
    try:
        match_w = s_frame.instances_array(w_class)
        if match_w is None:
            if some_instance and interp.space.is_spur.is_set():
                # on Spur, someInstance really means just one, it's not used to
                # start iterating over all instances
                return get_instances_array_trace(interp, w_class, some_instance=True)
            match_w = get_instances_array_trace(interp, w_class)
            if store:
                s_frame.store_instances_array(w_class, match_w)
        return match_w
    finally:
        active_process.store_suspended_context(interp.space.w_nil)


@expose_primitive(SOME_INSTANCE, unwrap_spec=[object])
def func(interp, s_frame, w_class):
    # This primitive returns some instance of the class on the stack.
    # If no class is given, it returns some object.
    if w_class.is_same_object(interp.space.w_SmallInteger):
        raise PrimitiveFailedError()

    match_w = get_instances_array(interp, s_frame, w_class=w_class, some_instance=True)
    try:
        return match_w[0]
    except IndexError:
        raise PrimitiveFailedError()

def next_instance(space, list_of_objects, w_obj):
    retval = None
    try:
        idx = list_of_objects.index(w_obj)
    except ValueError:
        idx = -1
    try:
        retval = list_of_objects[idx + 1]
    except IndexError:
        raise PrimitiveFailedError()
    # just in case, that one of the objects in the list changes its class
    if retval.getclass(space).is_same_object(w_obj.getclass(space)):
        return retval
    else:
        list_of_objects.pop(idx + 1)
        return next_instance(space, list_of_objects, w_obj)

@expose_primitive(NEXT_INSTANCE, unwrap_spec=[object])
def func(interp, s_frame, w_obj):
    # This primitive is used to iterate through all instances of a class:
    # it returns the "next" instance after w_obj.
    return next_instance(
        interp.space,
        get_instances_array(interp, s_frame, w_class=w_obj.getclass(interp.space)),
        w_obj
    )

@expose_primitive(NEW_METHOD, unwrap_spec=[object, int, int])
def func(interp, s_frame, w_class, bytecount, header):
    # We ignore w_class because W_CompiledMethod subclasses are special
    if interp.space.is_spur.is_set():
        return W_SpurCompiledMethod(interp.space, bytecount, header)
    else:
        return W_PreSpurCompiledMethod(interp.space, bytecount, header)

# ___________________________________________________________________________
# I/O Primitives

MOUSE_POINT = 90
TEST_DISPLAY_DEPTH = 91
SET_DISPLAY_MODE = 92
INPUT_SEMAPHORE = 93
GET_NEXT_EVENT = 94
INPUT_WORD = 95
BITBLT_COPY_BITS = 96
SNAPSHOT = 97
STORE_IMAGE_SEGMENT = 98
LOAD_IMAGE_SEGMENT = 99
PERFORM_IN_SUPERCLASS = 100
BE_CURSOR = 101
BE_DISPLAY = 102
SCAN_CHARACTERS = 103
OBSOLETE_INDEXED = 104  # also 96
STRING_REPLACE = 105
SCREEN_SIZE = 106
MOUSE_BUTTONS = 107
KBD_NEXT = 108
KBD_PEEK = 109

@expose_primitive(MOUSE_POINT, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    x, y = interp.space.display().mouse_point()
    w_point = W_PointersObject(interp.space, interp.space.w_Point, 2)
    w_point.store(interp.space, 0, interp.space.wrap_int(x))
    w_point.store(interp.space, 1, interp.space.wrap_int(y))
    return w_point

@expose_primitive(GET_NEXT_EVENT, unwrap_spec=[object, object])
@jit.unroll_safe
@jit.look_inside
def func(interp, s_frame, w_rcvr, w_into):
    if not interp.evented:
        raise PrimitiveFailedError()
    try:
        ary = interp.space.display().get_next_event(time=interp.event_time_now())
    except display.SqueakInterrupt, e:
        w_interrupt_sema = interp.space.objtable['w_interrupt_semaphore']
        if w_interrupt_sema is not interp.space.w_nil:
            assert_class(interp, w_interrupt_sema, interp.space.w_Semaphore)
            wrapper.SemaphoreWrapper(interp.space, w_interrupt_sema).signal(s_frame)
        else:
            raise e
    else:
        for i in range(8):
            w_into.store(interp.space, i, interp.space.wrap_int(ary[i]))
        # XXX - hack
        if ary[0] == display.WindowEventMetricChange and ary[4] > 0 and ary[5] > 0:
            if interp.image:
                interp.image.lastWindowSize = ((ary[4] & 0xffff) << 16) | (ary[5] & 0xffff)
    return w_rcvr

@expose_primitive(BITBLT_COPY_BITS, clean_stack=False, no_result=True,
                  compiled_method=True)
def func(interp, s_frame, argcount, w_method):
    w_name = interp.space.wrap_string("primitiveCopyBits")
    signature = ("BitBltPlugin", "primitiveCopyBits")
    from rsqueakvm.plugins.simulation import SimulationPlugin
    return SimulationPlugin.simulate(w_name, signature, interp, s_frame, argcount, w_method)

@expose_primitive(SNAPSHOT, clean_stack=False, no_result=True)
def func(interp, s_frame, argcount):
    s_frame.pop_n(argcount)
    s_frame.push(interp.space.w_true)
    # leaving true on the frame as return value for resuming image
    from rsqueakvm.squeakimage import SpurImageWriter
    from rsqueakvm.constants import SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX
    filename = interp.space.get_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX)
    SpurImageWriter(interp, filename).trace_image(s_frame)
    s_frame.pop()
    s_frame.push(interp.space.w_false)  # the non-resuming image gets false

@expose_primitive(BE_CURSOR)
def func(interp, s_frame, argcount):
    if not (0 <= argcount <= 1):
        raise PrimitiveFailedError()
    w_rcvr = s_frame.peek(argcount)

    if interp.space.headless.is_set():
        # we don't do any cursoration if we're headless
        return w_rcvr

    mask_words = None
    if argcount == 1:
        w_mask = s_frame.peek(0)
        if isinstance(w_mask, W_WordsObject):
            mask_words = w_mask.words
        elif isinstance(w_mask, W_PointersObject):
            # mask is a form object
            w_contents = w_mask.fetch(interp.space, 0)
            if isinstance(w_contents, W_WordsObject):
                mask_words = w_contents.words
            else:
                raise PrimitiveFailedError
        else:
            raise PrimitiveFailedError()
    w_bitmap = w_rcvr.fetch(interp.space, 0)
    if not isinstance(w_bitmap, W_WordsObject):
        raise PrimitiveFailedError()
    width = interp.space.unwrap_int(w_rcvr.fetch(interp.space, 1))
    height = interp.space.unwrap_int(w_rcvr.fetch(interp.space, 2))
    depth = interp.space.unwrap_int(w_rcvr.fetch(interp.space, 3))
    hotpt = wrapper.PointWrapper(interp.space, w_rcvr.fetch(interp.space, 4))
    offx = hotpt.x()
    offy = hotpt.y()
    if not (width == 16 and height == 16 and depth == 1 and
            offx >= -16 and offy >= -16 and
            offx <= 0 and offy <= 0):
        raise PrimitiveFailedError
    offx = -offx
    offy = -offy

    if display.SDLCursor.set(w_bitmap.words, width, height, offx, offy,
                             mask_words=mask_words):
        interp.space.objtable['w_cursor'] = w_rcvr
    # Don't fail if the Cursor could not be set.
    # It is surely annoying but no reason to not continue.
    s_frame.pop_n(argcount + 1)
    return w_rcvr

@expose_primitive(BE_DISPLAY, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if interp.space.headless.is_set():
        exitFromHeadlessExecution(s_frame)
    if not isinstance(w_rcvr, W_PointersObject) or w_rcvr.size() < 4:
        raise PrimitiveFailedError

    old_display = interp.space.objtable['w_display']
    if isinstance(old_display, W_DisplayBitmap):
        old_display.relinquish_display()
    interp.space.objtable['w_display'] = w_rcvr

    form = wrapper.FormWrapper(interp.space, w_rcvr)
    form.take_over_display()
    w_display_bitmap = form.get_display_bitmap()
    w_display_bitmap.take_over_display()
    w_display_bitmap.flush_to_screen()

    if interp.image:
        interp.image.lastWindowSize = (form.width() << 16) + form.height()
    return w_rcvr

# @expose_primitive(STRING_REPLACE, unwrap_spec=[object, index1_0, index1_0, object, index1_0])
# @jit.look_inside_iff(lambda interp, s_frame, w_rcvr, start, stop, w_replacement, repStart: jit.isconstant(stop) and jit.isconstant(start))
# def func(interp, s_frame, w_rcvr, start, stop, w_replacement, repStart):
#     """replaceFrom: start to: stop with: replacement startingAt: repStart
#     Primitive. This destructively replaces elements from start to stop in the
#     receiver starting at index, repStart, in the collection, replacement. Answer
#     the receiver. Range checks are performed in the primitive only. Essential
#     for Pharo Candle Symbols.
#     | index repOff |
#     repOff := repStart - start.
#     index := start - 1.
#     [(index := index + 1) <= stop]
#         whileTrue: [self at: index put: (replacement at: repOff + index)]"""
#     if (start < 0 or start - 1 > stop or repStart < 0):
#         raise PrimitiveFailedError()
#     # This test deliberately test for equal W_Object class. The Smalltalk classes
#     # might be different (e.g. Symbol and ByteString)
#     if w_rcvr.__class__ is not w_replacement.__class__:
#         raise PrimitiveFailedError
#     if (w_rcvr.size() - w_rcvr.instsize() <= stop
#             or w_replacement.size() - w_replacement.instsize() <= repStart + (stop - start)):
#         raise PrimitiveFailedError()
#     repOff = repStart - start
#     for i0 in range(start, stop + 1):
#         w_rcvr.atput0(interp.space, i0, w_replacement.at0(interp.space, repOff + i0))
#     return w_rcvr

@expose_primitive(SCREEN_SIZE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    w_res = interp.space.w_Point.as_class_get_shadow(interp.space).new(2)
    point = wrapper.PointWrapper(interp.space, w_res)
    display = interp.space.display()
    if display.width == 0:
        # We need to have the indirection via interp.image, because when the image
        # is saved, the display form size is always reduced to 240@120.
        if not interp.image:
            raise PrimitiveFailedError
        display.width = (interp.image.lastWindowSize >> 16) & 0xffff
        display.height = interp.image.lastWindowSize & 0xffff
    point.store_x(display.width)
    point.store_y(display.height)
    return w_res

@expose_primitive(MOUSE_BUTTONS, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    btn = interp.space.display().mouse_button()
    return interp.space.wrap_int(btn)

@expose_primitive(KBD_NEXT, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    code = interp.space.display().next_keycode()
    if code & 0xFF == 0:
        return interp.space.w_nil
    else:
        return interp.space.wrap_int(code)

@expose_primitive(KBD_PEEK, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    code = interp.space.display().peek_keycode()
    # TODO: how do old images handle CmdDot? See INTERRUPT_SEMAPHORE?
    if code & 0xFF == 0:
        return interp.space.w_nil
    else:
        return interp.space.wrap_int(code)


# ___________________________________________________________________________
# Control Primitives

EQUIVALENT = 110
CLASS = 111
BYTES_LEFT = 112
QUIT = 113
EXIT_TO_DEBUGGER = 114
CHANGE_CLASS = 115      # Blue Book: primitiveOopsLeft
COMPILED_METHOD_FLUSH_CACHE = 116
EXTERNAL_CALL = 117
SYMBOL_FLUSH_CACHE = 119

@expose_primitive(EQUIVALENT, unwrap_spec=[object, object])
def func(interp, s_frame, w_arg, w_rcvr):
    return interp.space.wrap_bool(w_arg.is_same_object(w_rcvr))

@expose_primitive(CLASS, unwrap_spec=None)
def func(interp, s_frame, argcount):
    w_obj = s_frame.pop()
    if argcount == 1:
        s_frame.pop()  # receiver, e.g. ContextPart>>objectClass:
    return w_obj.getclass(interp.space)

@expose_primitive(BYTES_LEFT, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    return fake_bytes_left(interp)

@expose_primitive(QUIT, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    from rsqueakvm.error import Exit
    raise Exit('Quit-Primitive called')

@expose_primitive(EXIT_TO_DEBUGGER, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if interp.space.headless.is_set():
        exitFromHeadlessExecution(s_frame, "EXIT_TO_DEBUGGER")
    raise PrimitiveNotYetWrittenError()

@expose_primitive(CHANGE_CLASS, unwrap_spec=[object, object], no_result=True)
def func(interp, s_frame, w_arg, w_rcvr):
    w_arg_class = w_arg.getclass(interp.space)
    w_rcvr_class = w_rcvr.getclass(interp.space)

    # We should fail if:

    # 1. Rcvr or arg are SmallIntegers
    if (w_arg_class.is_same_object(interp.space.w_SmallInteger) or
        w_rcvr_class.is_same_object(interp.space.w_SmallInteger)):
        raise PrimitiveFailedError()

    # 2. Rcvr is an instance of a compact class and argument isn't
    # or vice versa XXX we don't have to fail here, but for squeak it's a problem

    # 3. Format of rcvr is different from format of argument
    if ((isinstance(w_arg, W_PointersObject) and
         isinstance(w_rcvr, W_PointersObject)) or
        (isinstance(w_arg, W_BytesObject) and
         isinstance(w_rcvr, W_BytesObject)) or
        (isinstance(w_arg, W_WordsObject) and
         isinstance(w_rcvr, W_WordsObject))):
        w_rcvr.change_class(interp.space, w_arg_class)
        return w_rcvr
    else:
        # TODO: this should also work to change bytes to words and such
        raise PrimitiveNotYetWrittenError

@expose_primitive(EXTERNAL_CALL, clean_stack=False, no_result=True,
                  compiled_method=True)
def func(interp, s_frame, argcount, w_method):
    space = interp.space
    w_description = w_method.literalat0(space, 1)
    if not isinstance(w_description, W_PointersObject) or w_description.size() < 2:
        raise PrimitiveFailedError
    w_modulename = jit.promote(w_description.at0(space, 0))
    w_functionname = jit.promote(w_description.at0(space, 1))
    if w_modulename is space.w_nil:
        """
        CompiledMethod allInstances select: [:cm | cm primitive = 117 and: [cm literals first first isNil]].
        There are no interesting named module-less primitives among those 28
        found in Squeak 5. They either have proper fallback or just don't work on
        Cog either.
        """
        raise  PrimitiveFailedError

    if not (isinstance(w_modulename, W_BytesObject) and
            isinstance(w_functionname, W_BytesObject)):
        raise PrimitiveFailedError
    signature = (space.unwrap_string(w_modulename), space.unwrap_string(w_functionname))

    if (not constants.IS_64BIT) and interp.space.use_plugins.is_set():
        from rsqueakvm.plugins.squeak_plugin_proxy import IProxy, MissingPlugin
        try:
            return IProxy.call(signature, interp, s_frame, argcount, w_method)
        except MissingPlugin:
            pass

    if False: pass  # just elifs
    elif signature[0] == 'LargeIntegers':
        from rsqueakvm.plugins.large_integer import LargeIntegerPlugin
        return LargeIntegerPlugin.call(signature[1], interp, s_frame, argcount, w_method)
    elif signature[0] == 'MiscPrimitivePlugin':
        from rsqueakvm.plugins.misc import MiscPrimitivePlugin
        return MiscPrimitivePlugin.call(signature[1], interp, s_frame, argcount, w_method)
    elif signature[0] == "SocketPlugin":
        from rsqueakvm.plugins.socket import SocketPlugin
        return SocketPlugin.call(signature[1], interp, s_frame, argcount, w_method)
    elif signature[0] == "FilePlugin":
        from rsqueakvm.plugins.fileplugin import FilePlugin
        return FilePlugin.call(signature[1], interp, s_frame, argcount, w_method)
    elif signature[0] == "VMDebugging":
        from rsqueakvm.plugins.vmdebugging import DebuggingPlugin
        return DebuggingPlugin.call(signature[1], interp, s_frame, argcount, w_method)
    else:
        from rsqueakvm.plugins.simulation import SimulationPlugin
        return SimulationPlugin.simulate(w_functionname, signature, interp, s_frame, argcount, w_method)

@expose_primitive(COMPILED_METHOD_FLUSH_CACHE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if not isinstance(w_rcvr, W_CompiledMethod):
        raise PrimitiveFailedError()
    w_class = w_rcvr.compiled_in()
    if w_class:
        w_class = assert_pointers(w_class)
        w_class.as_class_get_shadow(interp.space).flush_method_caches()
    return w_rcvr

@objectmodel.specialize.arg(0)
def walk_gc_references(func, gcrefs):
    from rpython.rlib import rgc
    for gcref in gcrefs:
        if gcref and not rgc.get_gcflag_extra(gcref):
            try:
                rgc.toggle_gcflag_extra(gcref)
                func(gcref)
                walk_gc_references(func, rgc.get_rpy_referents(gcref))
            finally:
                rgc.toggle_gcflag_extra(gcref)

@objectmodel.specialize.arg(0)
def walk_gc_objects(func):
    from rpython.rlib import rgc
    walk_gc_references(func, rgc.get_rpy_roots())

@objectmodel.specialize.arg(0, 1)
def walk_gc_objects_of_type(type, func):
    from rpython.rlib import rgc
    def check_type(gcref):
        w_obj = rgc.try_cast_gcref_to_instance(type, gcref)
        if w_obj:
            func(w_obj)
    walk_gc_objects(check_type)

@expose_primitive(SYMBOL_FLUSH_CACHE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    # No need to do this, method dictionaries invalidate their traces as needed
    return w_rcvr

# ___________________________________________________________________________
# Miscellaneous Primitives (120-127)
CALLOUT_TO_FFI = 120
IMAGE_NAME = 121
NOOP = 122
VALUE_UNINTERRUPTABLY = 123
LOW_SPACE_SEMAPHORE = 124
SIGNAL_AT_BYTES_LEFT = 125
DEFER_UPDATES = 126
DRAW_RECTANGLE = 127

@expose_primitive(IMAGE_NAME)
def func(interp, s_frame, argument_count):
    from rsqueakvm.constants import SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX
    if argument_count == 0:
        s_frame.pop()
        return interp.space.wrap_string(interp.space.get_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX))
    elif argument_count == 1:
        w_arg = s_frame.pop()
        assert isinstance(w_arg, W_BytesObject)
        interp.space.set_system_attribute(SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, interp.space.unwrap_string(w_arg))
        interp.space.display().set_title(interp.space.window_title())
        return s_frame.pop()
    raise PrimitiveFailedError

@expose_primitive(LOW_SPACE_SEMAPHORE, unwrap_spec=[object, object])
def func(interp, s_frame, w_receiver, i):
    # dont know when the space runs out
    return w_receiver

@expose_primitive(SIGNAL_AT_BYTES_LEFT, unwrap_spec=[object, int])
def func(interp, s_frame, w_receiver, i):
    # dont know when the space runs out
    return w_receiver

@expose_primitive(DEFER_UPDATES, unwrap_spec=[object, bool])
def func(interp, s_frame, w_receiver, flag):
    sdldisplay = interp.space.display()
    sdldisplay.defer_updates(flag)
    return w_receiver

@expose_primitive(DRAW_RECTANGLE, unwrap_spec=[object, int, int, int, int])
def func(interp, s_frame, w_rcvr, left, right, top, bottom):
    if not interp.space.objtable['w_display'].is_same_object(w_rcvr):
        return interp.space.w_nil
    if not ((left <= right) and (top <= bottom)):
        return interp.space.w_nil
    form = wrapper.FormWrapper(interp.space, w_rcvr)
    form.get_display_bitmap().force_rectange_to_screen(left, right, top, bottom)
    interp.space.display().flip(force=True)
    return w_rcvr


# ___________________________________________________________________________
# Squeak Miscellaneous Primitives (128-134)
BECOME = 128
SPECIAL_OBJECTS_ARRAY = 129
FULL_GC = 130
INC_GC = 131
SET_INTERRUPT_KEY = 133
INTERRUPT_SEMAPHORE = 134

@expose_primitive(BECOME, unwrap_spec=[object, object])
def func(interp, s_frame, w_rcvr, w_new):
    if w_rcvr.size() != w_new.size():
        raise PrimitiveFailedError
    w_lefts = []
    w_rights = []
    for i in range(w_rcvr.size()):
        w_left = w_rcvr.at0(interp.space, i)
        w_right = w_new.at0(interp.space, i)
        if w_left.become(w_right):
            w_lefts.append(w_left)
            w_rights.append(w_right)
        else:
            for i in range(len(w_lefts)):
                w_lefts[i].become(w_rights[i])
            raise PrimitiveFailedError()
    return w_rcvr

def fake_bytes_left(interp):
    from rsqueakvm.util.platform_calls import get_memory_usage
    usage = get_memory_usage()
    if usage < 0:
        # there was an error getting the result
        return interp.space.wrap_int(2**29)
    else:
        return interp.space.wrap_int(constants.MAXINT - usage)

@expose_primitive(SPECIAL_OBJECTS_ARRAY, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    return interp.image.special_objects

@expose_primitive(INC_GC, unwrap_spec=[object])
@expose_primitive(FULL_GC, unwrap_spec=[object])
@jit.dont_look_inside
def func(interp, s_frame, w_rcvr):
    # Squeak pops the arg and ignores it ... go figure
    from rpython.rlib import rgc
    rgc.collect()
    return fake_bytes_left(interp)

@expose_primitive(SET_INTERRUPT_KEY, unwrap_spec=[object, int])
def func(interp, s_frame, w_rcvr, encoded_key):
    interp.space.display().set_interrupt_key(interp.space, encoded_key)
    return w_rcvr

@expose_primitive(INTERRUPT_SEMAPHORE, unwrap_spec=[object, object])
def func(interp, s_frame, w_rcvr, w_semaphore):
    if w_semaphore.getclass(interp.space).is_same_object(interp.space.w_Semaphore):
        interp.space.objtable['w_interrupt_semaphore'] = w_semaphore
    else:
        interp.space.objtable['w_interrupt_semaphore'] = interp.space.w_nil
    return w_rcvr

#____________________________________________________________________________
# Time Primitives (135 - 137, 240 - 242)
MILLISECOND_CLOCK = 135
SIGNAL_AT_MILLISECONDS = 136
SECONDS_CLOCK = 137

UTC_MICROSECOND_CLOCK = 240
LOCAL_MICROSECOND_CLOCK = 241
SIGNAL_AT_UTC_MICROSECONDS = 242
UPDATE_TIMEZONE = 243

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

@expose_primitive(SIGNAL_AT_UTC_MICROSECONDS, unwrap_spec=[object, object, r_int64])
def func(interp, s_frame, w_delay, w_semaphore, timestamp):
    if not w_semaphore.getclass(interp.space).is_same_object(
            interp.space.w_Semaphore):
        interp.space.objtable["w_timerSemaphore"] = interp.space.w_nil
    else:
        interp.space.objtable["w_timerSemaphore"] = w_semaphore
    interp.next_wakeup_tick = timestamp
    return w_delay

#____________________________________________________________________________
# Misc Primitives (138 - 149)
SOME_OBJECT = 138
NEXT_OBJECT = 139
BEEP = 140
CLIPBOARD_TEXT = 141
VM_PATH = 142
SHORT_AT = 143
SHORT_AT_PUT = 144
FILL = 145
CLONE = 148
SYSTEM_ATTRIBUTE = 149

@expose_primitive(SOME_OBJECT, unwrap_spec=[object])
def func(interp, s_frame, w_class):
    match_w = get_instances_array(interp, s_frame, some_instance=True)
    try:
        return match_w[0]
    except IndexError:
        raise PrimitiveFailedError()

def next_object(space, list_of_objects, w_obj):
    retval = None
    try:
        idx = list_of_objects.index(w_obj)
    except ValueError:
        idx = -1
    try:
        retval = list_of_objects[idx + 1]
    except IndexError:
        return space.wrap_int(0)
    return retval

@expose_primitive(NEXT_OBJECT, unwrap_spec=[object])
def func(interp, s_frame, w_obj):
    # This primitive is used to iterate through all objects:
    # it returns the "next" instance after w_obj.
    return next_object(interp.space, get_instances_array(interp, s_frame), w_obj)

@expose_primitive(BEEP, unwrap_spec=[object])
def func(interp, s_frame, w_receiver):
    return w_receiver

@expose_primitive(CLIPBOARD_TEXT)
def func(interp, s_frame, argument_count):
    if argument_count == 0:
        if interp.space.display().has_clipboard_text():
            return interp.space.wrap_string(interp.space.display().get_clipboard_text())
        else:
            return interp.space.wrap_string("")
    elif argument_count == 1:
        w_arg = s_frame.pop()
        interp.space.display().set_clipboard_text(interp.space.unwrap_string(w_arg))

@expose_primitive(VM_PATH, unwrap_spec=[object])
def func(interp, s_frame, w_receiver):
    return interp.space.wrap_string("%s%s" % (interp.space.executable_path(), os.path.sep))

@expose_primitive(SHORT_AT, unwrap_spec=[object, index1_0])
def func(interp, s_frame, w_receiver, n0):
    if not (isinstance(w_receiver, W_BytesObject)
            or isinstance(w_receiver, W_WordsObject)):
        raise PrimitiveFailedError
    return w_receiver.short_at0(interp.space, n0)

@expose_primitive(SHORT_AT_PUT, unwrap_spec=[object, index1_0, object])
def func(interp, s_frame, w_receiver, n0, w_value):
    if not (isinstance(w_receiver, W_BytesObject)
            or isinstance(w_receiver, W_WordsObject)):
        raise PrimitiveFailedError
    return w_receiver.short_atput0(interp.space, n0, w_value)

@expose_primitive(FILL, unwrap_spec=[object, pos_32bit_int])
def func(interp, s_frame, w_arg, new_value):
    space = interp.space
    if isinstance(w_arg, W_BytesObject):
        if new_value > 255:
            raise PrimitiveFailedError
        for i in xrange(w_arg.size()):
            w_arg.setchar(i, chr(new_value))
    elif isinstance(w_arg, W_WordsObject) or isinstance(w_arg, W_DisplayBitmap):
        for i in xrange(w_arg.size()):
            w_arg.setword(i, new_value)
    else:
        raise PrimitiveFailedError
    return w_arg

@expose_primitive(CLONE, unwrap_spec=[object])
def func(interp, s_frame, w_arg):
    return w_arg.clone(interp.space)

@expose_primitive(SYSTEM_ATTRIBUTE, unwrap_spec=[object, int])
def func(interp, s_frame, w_receiver, attr_id):
    try:
        return interp.space.wrap_string("%s" % interp.space.get_system_attribute(attr_id))
    except KeyError:
        return interp.space.w_nil

# ___________________________________________________________________________
# File primitives (150-169)
# (XXX they are obsolete in Squeak and done with a plugin)

FILE_AT_END = 150
FILE_CLOSE = 151
FILE_GET_POSITION = 152
FILE_OPEN = 153
FILE_READ = 154
FILE_SET_POSITION = 155
FILE_DELETE = 156
FILE_SIZE = 157
FILE_WRITE = 158
FILE_RENAME = 159
DIRECTORY_CREATE = 160
DIRECTORY_DELIMITOR = 161
DIRECTORY_LOOKUP = 162
DIRECTORY_DELTE = 163

@expose_primitive(FILE_CLOSE, unwrap_spec=[object, int])
def func(interp, s_frame, w_rcvr, fd):
    try:
        os.close(fd)
    except OSError:
        raise PrimitiveFailedError()
    return w_rcvr

@expose_primitive(FILE_OPEN, unwrap_spec=[object, str, object])
def func(interp, s_frame, w_rcvr, filename, w_writeable_flag):
    if w_writeable_flag.is_same_object(interp.space.w_true):
        mode = os.O_RDWR | os.O_CREAT | os.O_TRUNC
    else:
        mode = os.O_RDONLY
    try:
        fd = os.open(filename, mode, 0666)
    except OSError:
        raise PrimitiveFailedError()
    return interp.space.wrap_int(fd)

@expose_primitive(FILE_WRITE, unwrap_spec=[object, int, str, int, int])
def func(interp, s_frame, w_rcvr, fd, src, start, count):
    start = start - 1
    end = start + count
    if end < 0 or start < 0:
        raise PrimitiveFailedError()
    try:
        os.write(fd, src[start:end])
    except OSError:
        raise PrimitiveFailedError()
    return w_rcvr

@expose_primitive(DIRECTORY_DELIMITOR, unwrap_spec=[object])
def func(interp, s_frame, _):
    return interp.space.wrap_char(os.path.sep)


# ___________________________________________________________________________

@expose_primitive(CHARACTER_VALUE)
def func(interp, s_frame, argument_count):
    w_value = s_frame.peek(0)
    assert isinstance(w_value, W_SmallInteger)
    s_frame.pop_n(argument_count + 1)
    return W_Character(interp.space.unwrap_int(w_value))



# ___________________________________________________________________________
# Boolean Primitives

LESSTHAN = 3
GREATERTHAN = 4
LESSOREQUAL = 5
GREATEROREQUAL = 6
EQUAL = 7
NOTEQUAL = 8

LARGE_LESSTHAN = 23
LARGE_GREATERTHAN = 24
LARGE_LESSOREQUAL = 25
LARGE_GREATEROREQUAL = 26
LARGE_EQUAL = 27
LARGE_NOTEQUAL = 28

FLOAT_LESSTHAN = 43
FLOAT_GREATERTHAN = 44
FLOAT_LESSOREQUAL = 45
FLOAT_GREATEROREQUAL = 46
FLOAT_EQUAL = 47
FLOAT_NOTEQUAL = 48

bool_ops = {
    LESSTHAN: operator.lt,
    GREATERTHAN: operator.gt,
    LESSOREQUAL: operator.le,
    GREATEROREQUAL:operator.ge,
    EQUAL: operator.eq,
    NOTEQUAL: operator.ne
    }
for (code,op) in bool_ops.items():
    def make_func(op):
        @expose_also_as(code + _LARGE_OFFSET)
        @expose_primitive(code, unwrap_specs=combination_specs)
        def func(interp, s_frame, v1, v2):
            res = op(v1, v2)
            w_res = interp.space.wrap_bool(res)
            return w_res
    make_func(op)

for (code,op) in bool_ops.items():
    def make_func(op):
        @expose_primitive(code+_FLOAT_OFFSET, unwrap_spec=[float, float])
        def func(interp, s_frame, v1, v2):
            res = op(v1, v2)
            w_res = interp.space.wrap_bool(res)
            return w_res
    make_func(op)

# ___________________________________________________________________________
# Quick Push Const Primitives

PUSH_SELF = 256
PUSH_TRUE = 257
PUSH_FALSE = 258
PUSH_NIL = 259
PUSH_MINUS_ONE = 260
PUSH_ZERO = 261
PUSH_ONE = 262
PUSH_TWO = 263

@expose_primitive(PUSH_SELF, unwrap_spec=[object])
def func(interp, s_frame, w_self):
    # no-op really
    return w_self

def make_push_const_func(code, name):
    @expose_primitive(code, unwrap_spec=[object])
    def func(interp, s_frame, w_ignored):
        return getattr(interp.space, name)
    return func

for (code, name) in [
    (PUSH_TRUE, "w_true"),
    (PUSH_FALSE, "w_false"),
    (PUSH_NIL, "w_nil"),
    (PUSH_MINUS_ONE, "w_minus_one"),
    (PUSH_ZERO, "w_zero"),
    (PUSH_ONE, "w_one"),
    (PUSH_TWO, "w_two"),
    ]:
    make_push_const_func(code, name)

# ___________________________________________________________________________
# Control Primitives

BLOCK_COPY = 80
VALUE = 81
VALUE_WITH_ARGS = 82
PERFORM = 83
PERFORM_WITH_ARGS = 84
SIGNAL = 85
WAIT = 86
RESUME = 87
SUSPEND = 88
FLUSH_CACHE = 89
YIELD = 167

BYTE_SIZE_OF_INSTANCE = 181

EXIT_CRITICAL_SECTION = 185  # similar to SIGNAL, hence SIGNAL + 100
ENTER_CRITICAL_SECTION = 186  # similar to WAIT, hence WAIT + 100
TEST_AND_SET_OWNERSHIP_OF_CRITICAL_SECTION = 187

WITH_ARGS_EXECUTE_METHOD = 188

@expose_primitive(BLOCK_COPY, unwrap_spec=[object, int])
def func(interp, s_frame, w_context, argcnt):
    # From B.B.: If receiver is a MethodContext, then it becomes
    # the new BlockContext's home context.  Otherwise, the home
    # context of the receiver is used for the new BlockContext.
    # Note that in our impl, MethodContext.w_home == self
    w_context = assert_pointers(w_context)
    s_method_context = w_context.as_context_get_shadow(interp.space).s_home()

    # The block bytecodes are stored inline: so we skip past the
    # bytecodes to invoke this primitive to get to them.
    initialip = s_frame.pc() + 2
    s_new_context = storage_contexts.ContextPartShadow.build_block_context(interp.space, s_method_context, argcnt, initialip)
    return s_new_context.w_self()

@expose_primitive(VALUE, result_is_new_frame=True)
def func(interp, s_frame, argument_count):
    # argument_count does NOT include the receiver.
    # This means that for argument_count == 3 the stack looks like:
    #  3      2       1      Top
    #  Rcvr | Arg 0 | Arg1 | Arg 2
    #
    # Validate that we have a block on the stack and that it received
    # the proper number of arguments:
    w_block_ctx = s_frame.peek(argument_count)

    # XXX need to check this since VALUE is called on all sorts of objects.
    if not w_block_ctx.getclass(interp.space).is_same_object(
        interp.space.w_BlockContext):
        raise PrimitiveFailedError()

    w_block_ctx = assert_pointers(w_block_ctx)
    s_block_ctx = w_block_ctx.as_context_get_shadow(interp.space)

    exp_arg_cnt = s_block_ctx.expected_argument_count()
    if argument_count != exp_arg_cnt: # exp_arg_cnt doesn't count self
        raise PrimitiveFailedError()

    # Initialize the block stack with the arguments that were
    # pushed.  Also pop the receiver.
    block_args = s_frame.pop_and_return_n(exp_arg_cnt)

    # Reset stack of blockcontext to []
    s_block_ctx.reset_stack()
    s_block_ctx.push_all(block_args)

    s_frame.pop()
    s_block_ctx.reset_pc()
    return s_block_ctx

@expose_primitive(VALUE_WITH_ARGS, unwrap_spec=[object, list],
                  result_is_new_frame=True)
def func(interp, s_frame, w_block_ctx, args_w):

    w_block_ctx = assert_pointers(w_block_ctx)
    s_block_ctx = w_block_ctx.as_context_get_shadow(interp.space)
    exp_arg_cnt = s_block_ctx.expected_argument_count()

    if len(args_w) != exp_arg_cnt:
        raise PrimitiveFailedError()

    # Push all the items from the array
    for i in range(exp_arg_cnt):
        s_block_ctx.push(args_w[i])

    # XXX Check original logic. Image does not test this anyway
    # because falls back to value + internal implementation
    s_block_ctx.reset_pc()
    return s_block_ctx

@expose_primitive(PERFORM)
def func(interp, s_frame, argcount):
    raise PrimitiveFailedError()

@expose_primitive(PERFORM_WITH_ARGS,
                  unwrap_spec=[object, object, list],
                  no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr, w_selector, w_arguments):
    s_frame.pop_n(2)  # removing our arguments
    return s_frame._sendSelector(w_selector, len(w_arguments), interp, w_rcvr,
                        w_rcvr.class_shadow(interp.space), w_arguments=w_arguments)

@expose_primitive(WITH_ARGS_EXECUTE_METHOD,
    result_is_new_frame=True, unwrap_spec=[object, list, object])
def func(interp, s_frame, w_rcvr, args_w, w_cm):
    if not isinstance(w_cm, W_CompiledMethod):
        raise PrimitiveFailedError()
    code = w_cm.primitive()
    if code:
        raise PrimitiveFailedError("withArgs:executeMethod: not support with primitive method")
    return w_cm.create_frame(interp.space, w_rcvr, args_w)


# XXX we might want to disable the assert_class checks in the 4 primitives below

@expose_primitive(SIGNAL, unwrap_spec=[object], clean_stack=False, no_result=True)
def func(interp, s_frame, w_rcvr):
    assert_class(interp, w_rcvr, interp.space.w_Semaphore)
    wrapper.SemaphoreWrapper(interp.space, w_rcvr).signal(s_frame)

@expose_primitive(WAIT, unwrap_spec=[object], clean_stack=False, no_result=True)
def func(interp, s_frame, w_rcvr):
    assert_class(interp, w_rcvr, interp.space.w_Semaphore)
    wrapper.SemaphoreWrapper(interp.space, w_rcvr).wait(s_frame)

@expose_primitive(RESUME, unwrap_spec=[object], no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr):
    assert_class(interp, w_rcvr, interp.space.w_Process)
    wrapper.ProcessWrapper(interp.space, w_rcvr).resume(s_frame)

@expose_primitive(SUSPEND, unwrap_spec=[object], no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr):
    assert_class(interp, w_rcvr, interp.space.w_Process)
    proc = wrapper.ProcessWrapper(interp.space, w_rcvr)
    s_frame.pop()  # remove receiver
    s_frame.push(proc.my_list())  # leave my_list on stack as return value
    proc.suspend(s_frame)

@expose_primitive(YIELD, unwrap_spec=[object], no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr):
    # we leave the rcvr on the stack, so it is there even if we resume another
    # process when yielding
    w_process = wrapper.SchedulerWrapper(interp.space, w_rcvr).active_process()
    wrapper.ProcessWrapper(interp.space, w_process).yield_(s_frame)

@expose_primitive(EXIT_CRITICAL_SECTION, unwrap_spec=[object], clean_stack=False, no_result=True)
def func(interp, s_frame, w_rcvr):
    # we leave the rcvr on the stack, so it is there even if we resume another
    # process immediately when exiting the critical section
    wrapper.CriticalSectionWrapper(interp.space, w_rcvr).exit(s_frame)

@expose_primitive(ENTER_CRITICAL_SECTION, unwrap_spec=[object], clean_stack=False, no_result=True)
def func(interp, s_frame, w_rcvr):
    assert s_frame.pop() is w_rcvr
    # we take pop the receiver and push the return value of this primitive
    # inside the wrapper code, so it is there regardless of if we have to wait
    # or not
    return wrapper.CriticalSectionWrapper(interp.space, w_rcvr).enter(s_frame)

@expose_primitive(TEST_AND_SET_OWNERSHIP_OF_CRITICAL_SECTION, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    return wrapper.CriticalSectionWrapper(interp.space, w_rcvr).test_and_set_owner(s_frame)

@expose_primitive(FLUSH_CACHE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    w_rcvr = assert_pointers(w_rcvr)
    s_class = w_rcvr.as_class_get_shadow(interp.space)
    s_class.flush_method_caches()
    return w_rcvr

def model_sizeof(model):
    return 4 # constant for interpretation

from rpython.rtyper.extregistry import ExtRegistryEntry
class Entry(ExtRegistryEntry):
    _about_ = model_sizeof

    def compute_result_annotation(self, s_model):
        from rpython.annotator.model import SomeInteger
        return SomeInteger(nonneg=True, knowntype=int)

    def specialize_call(self, hop):
        from rpython.rtyper.lltypesystem import lltype
        from rpython.memory.lltypelayout import sizeof
        modelrepr = hop.rtyper.getrepr(hop.args_s[0])
        hop.exception_cannot_occur()
        sz = sizeof(modelrepr.lowleveltype.TO, 1)
        # Also add the instance-specific helper structs that all instances have
        if modelrepr.rclass.classdef.classdesc.pyobj is W_PointersObject:
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst__storage"].TO, 0)
            # not adding shadows, because these are often shared
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_WordsObject:
            sz += sizeof(modelrepr.lowleveltype.TO._flds["mutate_words"].TO)
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst_words"].TO, 0)
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_BytesObject:
            sz += sizeof(modelrepr.lowleveltype.TO._flds["mutate_version"].TO)
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_CompiledMethod:
            sz += sizeof(modelrepr.lowleveltype.TO._flds["mutate_version"].TO)
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst_literals"].TO, 0)
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst_version"].TO)
            sz += sizeof(modelrepr.lowleveltype.TO._flds["inst_lookup_selector"].TO, 0)
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_Float:
            pass
        elif modelrepr.rclass.classdef.classdesc.pyobj is W_LargePositiveInteger1Word:
            pass
        return hop.inputconst(lltype.Signed, sz)

@expose_primitive(BYTE_SIZE_OF_INSTANCE)
def func(interp, s_frame, argcount):
    from rpython.memory.lltypelayout import sizeof
    from rsqueakvm.storage_classes import POINTERS,\
        WEAK_POINTERS, WORDS, BYTES, COMPILED_METHOD,\
        FLOAT, LARGE_POSITIVE_INTEGER
    # This does not count shadows and the memory required for storage or any of
    # that "meta-info", but only the size of the types struct and the requested
    # fields.
    if argcount == 1:
        size = interp.space.unwrap_int(s_frame.pop())
    else:
        size = 0
    w_rcvr = s_frame.pop()

    if argcount > 1:
        raise PrimitiveFailedError

    s_class = w_rcvr.as_class_get_shadow(interp.space)
    instance_kind = s_class.get_instance_kind()
    if instance_kind in [POINTERS, WEAK_POINTERS]:
        r = model_sizeof(objectmodel.instantiate(W_PointersObject)) + (s_class.instsize() + size) * constants.BYTES_PER_WORD
    elif instance_kind == WORDS:
        r = model_sizeof(objectmodel.instantiate(W_WordsObject)) + size * constants.BYTES_PER_WORD
    elif instance_kind == BYTES:
        r = model_sizeof(objectmodel.instantiate(W_BytesObject)) + size
    elif instance_kind == COMPILED_METHOD:
        r = model_sizeof(objectmodel.instantiate(W_CompiledMethod))
    elif instance_kind == FLOAT:
        r = model_sizeof(objectmodel.instantiate(W_Float))
    elif instance_kind == LARGE_POSITIVE_INTEGER:
        if size <= 4:
            r = model_sizeof(objectmodel.instantiate(W_LargePositiveInteger1Word))
        else:
            r = model_sizeof(objectmodel.instantiate(W_BytesObject)) + size
    else:
        raise PrimitiveFailedError
    return interp.space.wrap_int(r)

# ___________________________________________________________________________
# BlockClosure Primitives

CLOSURE_COPY_WITH_COPIED_VALUES = 200
CLOSURE_VALUE = 201
CLOSURE_VALUE_ = 202
CLOSURE_VALUE_VALUE = 203
CLOSURE_VALUE_VALUE_VALUE = 204
CLOSURE_VALUE_VALUE_VALUE_VALUE = 205
CLOSURE_VALUE_WITH_ARGS = 206  #valueWithArguments:
CLOSURE_VALUE_NO_CONTEXT_SWITCH = 221
CLOSURE_VALUE_NO_CONTEXT_SWITCH_ = 222

@expose_primitive(CLOSURE_COPY_WITH_COPIED_VALUES, unwrap_spec=[object, int, list])
def func(interp, s_frame, outerContext, numArgs, copiedValues):
    w_context = interp.space.newClosure(outerContext, s_frame.pc(),
                                                        numArgs, copiedValues)
    return w_context


def activateClosure(interp, w_block, args_w):
    space = interp.space
    assert_class(interp, w_block, space.w_BlockClosure)
    block = wrapper.BlockClosureWrapper(space, w_block)
    blockNumArgs = jit.promote(block.numArgs())
    if not blockNumArgs == len(args_w):
        raise PrimitiveFailedError()
    outer_ctxt = block.outerContext()
    outer_ctxt_class = jit.promote(outer_ctxt.getclass(space))
    if not (outer_ctxt_class is space.w_MethodContext
                or outer_ctxt_class is space.w_BlockContext):
        raise PrimitiveFailedError()
    assert isinstance(outer_ctxt, W_PointersObject)

    # additionally to the smalltalk implementation, this also pushes
    # args and copiedValues
    s_new_frame = block.create_frame(outer_ctxt, args_w)
    w_closureMethod = s_new_frame.w_method()

    assert isinstance(w_closureMethod, W_CompiledMethod)
    assert w_block is not block.outerContext()

    return s_new_frame


@expose_primitive(CLOSURE_VALUE, unwrap_spec=[object], result_is_new_frame=True)
def func(interp, s_frame, w_block_closure):
    return activateClosure(interp, w_block_closure, [])

@expose_primitive(CLOSURE_VALUE_, unwrap_spec=[object, object], result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, w_a0):
    return activateClosure(interp, w_block_closure, [w_a0])

@expose_primitive(CLOSURE_VALUE_VALUE, unwrap_spec=[object, object, object], result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, w_a0, w_a1):
    return activateClosure(interp, w_block_closure, [w_a0, w_a1])

@expose_primitive(CLOSURE_VALUE_VALUE_VALUE, unwrap_spec=[object, object, object, object], result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, w_a0, w_a1, w_a2):
    return activateClosure(interp, w_block_closure, [w_a0, w_a1, w_a2])

@expose_primitive(CLOSURE_VALUE_VALUE_VALUE_VALUE, unwrap_spec=[object, object, object, object, object], result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, w_a0, w_a1, w_a2, w_a3):
    return activateClosure(interp, w_block_closure, [w_a0, w_a1, w_a2, w_a3])

@expose_primitive(CLOSURE_VALUE_WITH_ARGS, unwrap_spec=[object, list], result_is_new_frame=True)
def func(interp, s_frame, w_block_closure, args_w):
    return activateClosure(interp, w_block_closure, args_w)

@expose_primitive(CLOSURE_VALUE_NO_CONTEXT_SWITCH, unwrap_spec=[object], result_is_new_frame=True, may_context_switch=False)
def func(interp, s_frame, w_block_closure):
    return activateClosure(interp, w_block_closure, [])

@expose_primitive(CLOSURE_VALUE_NO_CONTEXT_SWITCH_, unwrap_spec=[object, object], result_is_new_frame=True, may_context_switch=False)
def func(interp, s_frame, w_block_closure, w_a0):
    return activateClosure(interp, w_block_closure, [w_a0])

# ___________________________________________________________________________
# Override the default primitive to give latitude to the VM in context management.

CTXT_AT = 210
CTXT_AT_PUT = 211
CTXT_SIZE = 212

@expose_primitive(CTXT_SIZE, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if isinstance(w_rcvr, W_PointersObject):
        if w_rcvr.getclass(interp.space).is_same_object(interp.space.w_MethodContext):
            if w_rcvr.fetch(interp.space, constants.MTHDCTX_METHOD) is interp.space.w_nil:
                # special case: (MethodContext allInstances at: 1) does not have a method. All fields are nil
                return interp.space.wrap_int(0)
            else:
                return interp.space.wrap_int(w_rcvr.as_context_get_shadow(interp.space).stackdepth())
    return interp.space.wrap_int(w_rcvr.varsize())

prim_table[CTXT_AT] = prim_table[AT]
prim_table[CTXT_AT_PUT] = prim_table[AT_PUT]
# ___________________________________________________________________________
# Drawing

IDLE_FOR_MICROSECONDS = 230
FORCE_DISPLAY_UPDATE = 231
SET_FULL_SCREEN = 233

@expose_primitive(IDLE_FOR_MICROSECONDS, unwrap_spec=[object, int], no_result=True, clean_stack=False)
def func(interp, s_frame, w_rcvr, time_mu_s):
    import time
    s_frame.pop()
    time_s = time_mu_s / 1000000.0
    interp.interrupt_check_counter = 0
    interp.quick_check_for_interrupt(s_frame, dec=0)
    time.sleep(time_s)
    interp.interrupt_check_counter = 0
    interp.quick_check_for_interrupt(s_frame, dec=0)

# @expose_primitive(FORCE_DISPLAY_UPDATE, unwrap_spec=[object])
# def func(interp, s_frame, w_rcvr):
#     interp.space.display().flip(force=True)
#     return w_rcvr

@expose_primitive(SET_FULL_SCREEN, unwrap_spec=[object, bool])
def func(interp, s_frame, w_rcvr, flag):
    interp.space.display().set_full_screen(flag)
    return w_rcvr

# ___________________________________________________________________________
# VM implementor primitives
VM_CLEAR_PROFILE = 250
VM_CONTROL_PROFILING = 251
VM_PROFILE_SAMPLES_INTO = 252
VM_PROFILE_INFO_INTO = 253
VM_PARAMETERS = 254
META_PRIM_FAILED = 255 # Used to be INST_VARS_PUT_FROM_STACK. Never used except in Disney tests.  Remove after 2.3 release.
VM_LOADED_MODULES = 573

@expose_primitive(META_PRIM_FAILED, unwrap_spec=[object, int])
def func(interp, s_frame, w_rcvr, primFailFlag):
    if primFailFlag != 0:
        raise MetaPrimFailed(s_frame, primFailFlag)
    raise PrimitiveFailedError

@expose_primitive(VM_PARAMETERS)
def func(interp, s_frame, argcount):
    """Behaviour depends on argument count:
            0 args: return an Array of VM parameter values;
            1 arg:  return the indicated VM parameter;
            2 args: set the VM indicated parameter.
        VM parameters are numbered as follows:
            1   byte size of old-space (read-only)
            2   byte size of young-space (read-only)
            3   byte size of object memory (read-only)
            4   allocationCount (read-only; nil in Cog VMs)
            5   allocations between GCs (read-write; nil in Cog VMs)
            6   survivor count tenuring threshold (read-write)
            7   full GCs since startup (read-only)
            8   total milliseconds in full GCs since startup (read-only)
            9   incremental GCs since startup (read-only; scavenging GCs on Spur)
            10  total milliseconds in incremental/scavenging GCs since startup (read-only)
            11  tenures of surving objects since startup (read-only)
            12-20 specific to the translating VM (nil in Cog VMs)
            21  root table size (read-only)
            22  root table overflows since startup (read-only)
            23  bytes of extra memory to reserve for VM buffers, plugins, etc.
            24  memory threshold above which to shrink object memory (read-write)
            25  ammount to grow by when growing object memory (read-write)
            26  interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
            27  number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
            28  number of times sweep loop iterated for current IGC/FGC (read-only)
            29  number of times make forward loop iterated for current IGC/FGC (read-only)
            30  number of times compact move loop iterated for current IGC/FGC (read-only)
            31  number of grow memory requests (read-only)
            32  number of shrink memory requests (read-only)
            33  number of root table entries used for current IGC/FGC (read-only)
            34  number of allocations done before current IGC/FGC (read-only)
            35  number of survivor objects after current IGC/FGC (read-only)
            36  millisecond clock when current IGC/FGC completed (read-only)
            37  number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
            38  milliseconds taken by current IGC (read-only)
            39  Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
            40  BytesPerWord for this image
            41  imageFormatVersion for the VM
            42  number of stack pages in use (Cog Stack VM only, otherwise nil)
            43  desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
            44  size of eden, in bytes (Cog VMs only, otherwise nil)
            45  desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
            46  size of machine code zone, in bytes (stored in image file header; Cog JIT VM only, otherwise nil)
            47  desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
            48  various properties of the Cog VM as an integer encoding an array of bit flags.
                Bit 0: implies the image's Process class has threadId as its 3rd inst var (zero relative)
                Bit 1: on Cog VMs asks the VM to set the flag bit in interpreted methods
                Bit 2: if set, preempting a process puts it to the head of its run queue, not the back,
                        i.e. preempting a process by a higher one will not cause the process to yield
                            to others at the same priority.
            49  the size of the external semaphore table (read-write; Cog VMs only)
            50-53 reserved for VM parameters that persist in the image (such as eden above)
            54  total size of free old space (Spur only, otherwise nil)
            55  ratio of growth and image size at or above which a GC will be performed post scavenge (Spur only, otherwise nil)
            56  number of process switches since startup (read-only)
            57  number of ioProcessEvents calls since startup (read-only)
            58  number of forceInterruptCheck (Cog VMs) or quickCheckInterruptCalls (non-Cog VMs) calls since startup (read-only)
            59  number of check event calls since startup (read-only)
            60  number of stack page overflows since startup (read-only; Cog VMs only)
            61  number of stack page divorces since startup (read-only; Cog VMs only)
            62  number of machine code zone compactions since startup (read-only; Cog VMs only)
            63  milliseconds taken by machine code zone compactions since startup (read-only; Cog VMs only)
            64  current number of machine code methods (read-only; Cog VMs only)
            65  true if the VM supports multiple bytecode sets;  (read-only; Cog VMs only; nil in older Cog VMs)
            66  the byte size of a stack page in the stack zone  (read-only; Cog VMs only)
            67 - 69 reserved for more Cog-related info
            70  the value of VM_PROXY_MAJOR (the interpreterProxy major version number)
            71  the value of VM_PROXY_MINOR (the interpreterProxy minor version number)

        Note: Thanks to Ian Piumarta for this primitive."""

    if not 0 <= argcount <= 2:
        raise PrimitiveFailedError

    arg1_w = s_frame.pop()  # receiver

    vm_w_params = [interp.space.wrap_int(0)] * 71

    vm_w_params[2] = interp.space.wrap_int(1)  # must be 1 for VM Stats view to work
    vm_w_params[8] = interp.space.wrap_int(1)  # must be 1 for VM Stats view to work

    vm_w_params[41] = interp.space.wrap_int(1)  # We are a "stack-like" VM - number of stack tables
    vm_w_params[45] = interp.space.wrap_int(1)  # We are a "cog-like" VM - machine code zone size

    vm_w_params[39] = interp.space.wrap_int(constants.BYTES_PER_WORD)
    vm_w_params[40] = interp.space.wrap_int(interp.image.version.magic)
    vm_w_params[55] = interp.space.wrap_int(interp.process_switch_count)
    vm_w_params[57] = interp.space.wrap_int(interp.forced_interrupt_checks_count)
    vm_w_params[59] = interp.space.wrap_int(interp.stack_overflow_count)
    vm_w_params[69] = interp.space.wrap_int(constants.INTERP_PROXY_MAJOR)
    vm_w_params[70] = interp.space.wrap_int(constants.INTERP_PROXY_MINOR)

    if argcount == 0:
        return interp.space.wrap_list(vm_w_params)

    arg2_w = s_frame.pop()  # index (really the receiver, index has been removed above)
    if not isinstance(arg1_w, W_SmallInteger):
        raise PrimitiveFailedError
    if argcount == 1:
        if not 0 <= arg1_w.value <= 70:
            raise PrimitiveFailedError
        return vm_w_params[arg1_w.value - 1]

    s_frame.pop()  # new value
    if argcount == 2:
        # return the 'old value'
        return interp.space.wrap_int(0)

# list the n-th loaded module
@expose_primitive(VM_LOADED_MODULES, unwrap_spec=[int])
def func(interp, s_frame, index):
    if interp.space.use_plugins.is_set():
        from rsqueakvm.plugins.squeak_plugin_proxy import IProxy
        modulenames = IProxy.loaded_module_names()
        try:
            return interp.space.wrap_string(modulenames[index])
        except IndexError:
            return interp.space.w_nil
    return interp.space.w_nil

# ___________________________________________________________________________
# PrimitiveLoadInstVar
#
# These are some wacky bytecodes in squeak.  They are defined to do
# the following:
#   primitiveLoadInstVar
#     | thisReceiver |
#     thisReceiver := self popStack.
#     self push: (self fetchPointer: primitiveIndex-264 ofObject: thisReceiver)

for i in range(264, 520):
    def make_prim(i):
        @expose_primitive(i, unwrap_spec=[object])
        def func(interp, s_frame, w_object):
            return w_object.fetch(interp.space, i - 264)
    globals()["INST_VAR_AT_%d" % (i-264)] = i
    make_prim(i)

unrolling_prim_table = unroll.unrolling_iterable(prim_table_implemented_only)
