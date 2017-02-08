"""Immutable W_PointersObject Implementation."""

from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.storage_classes import ClassShadow
from rsqueakvm.plugins.immutability.utils import immutable_class

from rpython.rlib import rerased
from rpython.rlib.unroll import unrolling_iterable

STORAGE_ATTR_TEMPLATE = "storage_%d"


@immutable_class
class W_AbstractImmutable_PointersObject(W_PointersObject):
    _attrs_ = []
    _immutable_fields_ = []
    repr_classname = ('%s_AbstractImmutable' %
                      W_PointersObject.repr_classname)

    def __init__(self, space, w_cls):
        W_AbstractObjectWithIdentityHash.__init__(self)
        # reuse strategy var to store cls
        self.strategy = w_cls.as_class_get_shadow(space)

    def getclass(self, space):
        return self.strategy.w_self()

    def class_shadow(self, space):
        class_shadow = self.strategy
        assert isinstance(class_shadow, ClassShadow)
        return class_shadow

    def size(self):
        raise NotImplementedError

    def fetch(self, space, n0):
        raise NotImplementedError


class W_Immutable_PointersObject(W_AbstractImmutable_PointersObject):
    _immutable_fields_ = ['_storage[*]']
    repr_classname = '%s_Immutable_N' % W_PointersObject.repr_classname
    erase, unerase = rerased.new_erasing_pair('storage_eraser')

    def __init__(self, space, w_cls, pointers_w):
        W_AbstractImmutable_PointersObject.__init__(self, space, w_cls)
        self._storage = self.erase(pointers_w)

    def size(self):
        return len(self.unerase(self._storage))

    def fetch(self, space, n0):
        return self.unerase(self._storage)[n0]


def generate_fixed_w_pointersobject_class(n_storage):
    storage_iter = unrolling_iterable(range(n_storage))
    cls_name = '%s_Immutable_%s' % (W_PointersObject.repr_classname, n_storage)

    class W_FixedImmutable_PointersObject(W_AbstractImmutable_PointersObject):
        _storages_ = [(STORAGE_ATTR_TEMPLATE % x) for x in storage_iter]
        _attrs_ = _storages_
        _immutable_fields_ = _storages_
        repr_classname = cls_name

        def __init__(self, space, w_cls, pointers_w):
            W_AbstractImmutable_PointersObject.__init__(self, space, w_cls)
            for x in storage_iter:
                setattr(self, STORAGE_ATTR_TEMPLATE % x, pointers_w[x])

        def size(self):
            return n_storage

        def fetch(self, space, n0):
            for x in storage_iter:
                if x == n0:
                    return getattr(self, STORAGE_ATTR_TEMPLATE % x)
            raise IndexError
            return self.storage[n0]

    W_FixedImmutable_PointersObject.__name__ = cls_name

    return W_FixedImmutable_PointersObject

pointers_classes = []
for n_storage in range(0, 10):
    pointers_classes.append(generate_fixed_w_pointersobject_class(n_storage))
pointers_classes.append(W_Immutable_PointersObject)
pointers_class_iter = unrolling_iterable(enumerate(pointers_classes))


def select_immutable_pointers_class(storage):
    length = len(storage)
    for i, cls in pointers_class_iter:
        if i == length:
            return cls
    # otherwise:
    return pointers_classes[-1]
