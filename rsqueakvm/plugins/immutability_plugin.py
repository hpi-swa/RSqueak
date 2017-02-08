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
    """
    Tests if w_recv is immutable.

    :param interp: The interpreter proxy.
    :param s_frame: The stack frame.
    :param w_recv: The receiver object.
    :returns: Return w_true if w_recv is immutable object. Returns w_false otherwise.
    :raises: *nothing*
    """
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object, object])
def primitiveImmutableFrom(interp, s_frame, w_cls, w_obj):
    """
    Creates an immutable copy of a Smalltalk object.

    :param interp: The interpreter proxy.
    :param s_frame: The stack frame.
    :param w_cls: The imutable objects target class.
    :param w_obj: The Smalltalk object to produce an immutable copy from.
    :returns: Immutable copy of w_obj with class w_cls.
    :raises: PrimitiveFailedError
    """
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
    """
    Creates an immutable object with class and arguments from stack frame.

    :param interp: The interpreter proxy.
    :param s_frame: The stack frame.
    :param argcount: The number of arguments.
    :returns: Immutable object with class and arguments from stack frame.
    :raises: PrimitiveFailedError
    """
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
