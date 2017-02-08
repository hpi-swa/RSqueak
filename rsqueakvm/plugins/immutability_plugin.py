"""
Documentation goes here.

"""

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.plugins.immutability.utils import patch_w_object

from rsqueakvm.storage_classes import BYTES, POINTERS, WORDS

from rsqueakvm.plugins.immutability.bytes import W_Immutable_BytesObject
from rsqueakvm.plugins.immutability.pointers import (
    select_immutable_pointers_class)
from rsqueakvm.plugins.immutability.words import W_Immutable_WordsObject


ImmutabilityPlugin = Plugin()
patch_w_object()


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object])
def primitiveIsImmutable(interp, s_frame, w_recv):
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object, object])
def primitiveImmutableFrom(interp, s_frame, w_cls, w_obj):
    space = interp.space
    instance_kind = w_cls.as_class_get_shadow(space).get_instance_kind()

    if instance_kind == POINTERS:
        pointers = w_obj.fetch_all(space)
        cls = select_immutable_pointers_class(pointers)
        return cls(space, w_cls, pointers)
    elif instance_kind == BYTES and isinstance(w_obj, W_BytesObject):
        return W_Immutable_BytesObject(space, w_cls, w_obj.bytes)
    elif instance_kind == WORDS and isinstance(w_obj, W_WordsObject):
        return W_Immutable_WordsObject(space, w_cls, w_obj.words)

    raise PrimitiveFailedError


@ImmutabilityPlugin.expose_primitive(unwrap_spec=None)
def primitiveImmutableFromArgs(interp, s_frame, argcount):

    w_arguments = s_frame.pop_and_return_n(argcount)[:]
    w_cls = s_frame.pop()
    space = interp.space
    instance_kind = w_cls.as_class_get_shadow(space).get_instance_kind()

    if instance_kind == POINTERS:
        cls = select_immutable_pointers_class(w_arguments)
        return cls(space, w_cls, w_arguments)
    # TBD:
    # elif instance_kind == BYTES and isinstance(w_obj, W_BytesObject):
    #     return W_Immutable_BytesObject(space, w_cls, w_obj.bytes)
    # elif instance_kind == WORDS and isinstance(w_obj, W_WordsObject):
    #     return W_Immutable_WordsObject(space, w_cls, w_obj.words)

    raise PrimitiveFailedError
