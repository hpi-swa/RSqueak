"""
RSqueak/VM plugin which provides support for immutable objects.

Immutable objects can be created as copy of existing objects
or from a list of arguments. The package `ImmutableObjects`, located in
`/repository`, needs to be loaded in the image.
"""

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.plugins.immutability import patch_w_object
from rsqueakvm.plugins.immutability.bytes import W_Immutable_BytesObject
from rsqueakvm.plugins.immutability.pointers import (
    select_immutable_pointers_class)
from rsqueakvm.plugins.immutability.words import W_Immutable_WordsObject
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.storage_classes import BYTES, POINTERS, WORDS


ImmutabilityPlugin = Plugin()
patch_w_object()


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object])
def primitiveIsImmutable(interp, s_frame, w_recv):
    """
    Tests if `w_recv` is an immutable object.

    :param interp: The interpreter proxy.
    :param s_frame: The stack frame.
    :param w_recv: The receiver object.
    :returns: `w_true` if `w_recv` is immutable object, otherwise `w_false`.
    """
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object, object])
def primitiveImmutableFrom(interp, s_frame, w_cls, w_obj):
    """
    Creates an immutable copy of a given Smalltalk object.

    :param interp: The interpreter proxy.
    :param s_frame: The stack frame.
    :param w_cls: The imutable objects target class.
    :param w_obj: The Smalltalk object to produce an immutable copy from.
    :returns: An immutable copy of `w_obj` with class `w_cls`.
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
    Returns an immutable instance of the receiver (which is a class) with
    all fields initialized with the arguments given.

    :param interp: The interpreter proxy.
    :param s_frame: The stack frame.
    :param argcount: The number of arguments.
    :returns: An immutable object.
    :raises: PrimitiveFailedError
    """
    if argcount == 0:
        raise PrimitiveFailedError
    w_args = s_frame.pop_and_return_n(argcount)[:]
    w_cls = s_frame.pop()
    space = interp.space
    instance_kind = w_cls.as_class_get_shadow(space).get_instance_kind()

    if instance_kind == POINTERS:
        cls = select_immutable_pointers_class(w_args)
        return cls(space, w_cls, w_args)
    elif (instance_kind == BYTES and
          len(w_args) == 1 and isinstance(w_args[0], W_BytesObject)):
        return W_Immutable_BytesObject(space, w_cls, w_args[0].bytes)
    elif (instance_kind == WORDS and
          len(w_args) == 1 and isinstance(w_args[0], W_WordsObject)):
        return W_Immutable_WordsObject(space, w_cls, w_args[0].words)

    raise PrimitiveFailedError
