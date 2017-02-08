from rsqueakvm.model.base import W_AbstractObjectWithClassReference
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.immutability.utils import immutable_class

from rpython.rlib import jit


@immutable_class
class W_Immutable_BytesObject(W_BytesObject):
    repr_classname = '%s_Immutable' % W_BytesObject.repr_classname

    def __init__(self, space, w_cls, bytes_w):
        W_AbstractObjectWithClassReference.__init__(self, space, w_cls)
        self.bytes = bytes_w

    @jit.elidable
    def _bytes(self):
        return W_BytesObject._bytes(self)

    def _version(self):
        return None
