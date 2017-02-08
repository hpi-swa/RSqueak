"""Immutable W_BytesObject Implementation."""

from rsqueakvm.model.base import W_AbstractObjectWithClassReference
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.immutability.utils import immutable_class

from rpython.rlib import jit


@immutable_class
class W_Immutable_BytesObject(W_BytesObject):
    _immutable_fields_ = ['immutable_bytes']
    repr_classname = '%s_Immutable' % W_BytesObject.repr_classname

    def __init__(self, space, w_cls, bytes_w):
        W_AbstractObjectWithClassReference.__init__(self, space, w_cls)
        self.immutable_bytes = bytes_w

    @jit.elidable
    def _bytes(self):
        return self.immutable_bytes

    def _version(self):
        return None

    """
    No need to override other methods that reference self.bytes, because they
    were stubbed out by @immutable_class.
    """
