from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.primitives import expose_primitive, assert_valid_index, index1_0
from rsqueakvm.primitives.constants import *

# ___________________________________________________________________________
# Array and Stream Primitives

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

@expose_primitive(SHORT_AT, unwrap_spec=[object, index1_0])
def func(interp, s_frame, w_receiver, n0):
    if not (isinstance(w_receiver, W_BytesObject) or
            isinstance(w_receiver, W_WordsObject)):
        raise PrimitiveFailedError
    return w_receiver.short_at0(interp.space, n0)

@expose_primitive(SHORT_AT_PUT, unwrap_spec=[object, index1_0, object])
def func(interp, s_frame, w_receiver, n0, w_value):
    if not (isinstance(w_receiver, W_BytesObject) or
            isinstance(w_receiver, W_WordsObject)):
        raise PrimitiveFailedError
    w_receiver.short_atput0(interp.space, n0, w_value)
    return w_value
