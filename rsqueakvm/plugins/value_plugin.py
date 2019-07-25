"""
RSqueak/VM plugin which provides support for compound values.

Values can be created as copy of existing objects
or from a list of arguments. The package `Values`, located in
`/repository`, needs to be loaded in the image.
"""

from rsqueakvm.error import PrimitiveFailedError, UnwrappingError
# from rsqueakvm.model.variable import W_BytesObject, W_WordsObject
from rsqueakvm.plugins.value import make_sure_all_value, NoValueError
# from rsqueakvm.plugins.value.bytes import W_Value_BytesObject # 
from rsqueakvm.plugins.value.pointers import W_PointersValue
# from rsqueakvm.plugins.value.words import W_Value_WordsObject
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.storage_classes import BYTES, POINTERS, WORDS


class ValuePlugin(Plugin):
    def setup(self):
        from rsqueakvm.plugins.value import patch
        patch()
    def patch(self):
        from rsqueakvm.plugins.value import patch_interpreter
        patch_interpreter()


plugin = ValuePlugin()



@plugin.expose_primitive(unwrap_spec=[object])
def primitiveIsValue(interp, s_frame, w_recv):
    """
    Tests if `w_recv` is a value.

    :param interp: The interpreter proxy.
    :param s_frame: The stack frame.
    :param w_recv: The receiver object.
    :returns: `w_true` if `w_recv` is a value.
    """
    if w_recv.is_value():
        return interp.space.w_true
    return interp.space.w_false


@plugin.expose_primitive(unwrap_spec=[object, object])
def primitiveValueFrom(interp, s_frame, w_cls, w_obj):
    """
    Creates a value of a given Smalltalk object by copying.

    :param interp: The interpreter proxy.
    :param s_frame: The stack frame.
    :param w_cls: The imutable objects target class.
    :param w_obj: The Smalltalk object to produce an immutable copy from.
    :returns: A copy of `w_obj` as value with class `w_cls`.
    :raises: PrimitiveFailedError
    """
    space = interp.space
    instance_kind = w_cls.as_class_get_shadow(space).get_instance_kind()

    if instance_kind == POINTERS:
        pointers_w = w_obj.fetch_all(space)
        try:
            vals_w = make_sure_all_value(pointers_w)
        except NoValueError:
            # print "DBG: No Value Error"
            raise PrimitiveFailedError
        # print vals_w
        return W_PointersValue.make(vals_w, space, w_cls)
    # elif instance_kind == BYTES and isinstance(w_obj, W_BytesObject):
    #     return W_Value_BytesObject(space, w_cls, w_obj.bytes)
    # elif instance_kind == WORDS and isinstance(w_obj, W_WordsObject):
    #     return W_Value_WordsObject(space, w_cls, w_obj.words)

    raise PrimitiveFailedError


@plugin.expose_primitive(unwrap_spec=None)
def primitiveValueFromArgs(interp, s_frame, argcount):
    """
    Returns an immutable instance of the receiver (which is a class) with
    all fields initialized with the arguments given.

    :param interp: The interpreter proxy.
    :param s_frame: The stack frame.
    :param argcount: The number of arguments.
    :returns: An immutable object.
    :raises: PrimitiveFailedError
    """
    args_w = s_frame.pop_and_return_n(argcount)[:]
    w_cls = s_frame.pop()
    space = interp.space
    instance_kind = w_cls.as_class_get_shadow(space).get_instance_kind()

    if instance_kind == POINTERS:
        try:
            vals_w = make_sure_all_value(args_w)
        except NoValueError:
            # print "DBG: No Value Error"
            raise PrimitiveFailedError
        # print vals_w
        return W_PointersValue.make(vals_w, space, w_cls)
    # elif instance_kind == BYTES:
    #     try:
    #         bytes = [chr(interp.space.unwrap_uint(b)) for b in w_args]
    #     except (ValueError, TypeError, UnwrappingError):
    #         raise PrimitiveFailedError
    #     return W_Value_BytesObject(space, w_cls, bytes)
    # elif instance_kind == WORDS:
    #     try:
    #         words = [interp.space.unwrap_uint(b) for b in w_args]
    #     except UnwrappingError:
    #         raise PrimitiveFailedError
    #     return W_Value_WordsObject(space, w_cls, words)

    raise PrimitiveFailedError
