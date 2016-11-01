import inspect

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.base import W_Object
from rsqueakvm.model.character import W_Character
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.primitives.constants import *

from rpython.rlib import unroll, jit, objectmodel
from rpython.rlib.rarithmetic import r_uint, r_int64, int_between
from rpython.rlib.rbigint import rbigint


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
bytelist = object()


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
                    if spec is r_int64:
                        args += (interp.space.unwrap_longlong(w_arg),)
                    elif spec is rbigint:
                        args += (interp.space.unwrap_rbigint(w_arg),)
                    elif spec is int:
                        args += (interp.space.unwrap_int(w_arg), )
                    elif spec is pos_32bit_int:
                        args += (interp.space.unwrap_positive_wordsize_int(w_arg),)
                    elif spec is r_uint:
                        args += (interp.space.unwrap_uint(w_arg),)
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
                    elif spec is bytelist:
                        if not isinstance(w_arg, W_BytesObject):
                            raise PrimitiveFailedError
                        args += (w_arg.getbytes(), )
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

prim_holder = PrimitiveHolder()
prim_holder.prim_table = prim_table
# clean up namespace:
del i
prim_table_implemented_only = []

# import for side effects
from rsqueakvm.primitives import arithmetic
from rsqueakvm.primitives import array_stream
from rsqueakvm.primitives import block_closure
from rsqueakvm.primitives import control
from rsqueakvm.primitives import input_output
from rsqueakvm.primitives import misc
from rsqueakvm.primitives import storage
from rsqueakvm.primitives import system

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
