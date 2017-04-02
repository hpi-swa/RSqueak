"""
Immutable W_PointersObject Implementation.

.. data:: POINTERS_CLASSES
A list of all immutable W_PointersObject subclasses. The position of each class
in the list correlates to its number of storage slots (`0` no storage, `1` has
one storage slot, ...). The last class in the list is an immutable
W_PointersObject subclass with variable storage size.

.. data:: POINTERS_CLASS_ITER
Unrolling iterable of `POINTERS_CLASSES`.

.. data:: STORAGE_ATTR_TEMPLATE
Storage attribute template.

.. data:: MAX_FIXED_SLOTS
Number of immutable subclasses with fixed number of slots to generate.

"""

from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.plugins.immutability import immutable_class
from rsqueakvm.storage_classes import ClassShadow

from rpython.rlib import rerased
from rpython.rlib.unroll import unrolling_iterable

STORAGE_ATTR_TEMPLATE = "storage_%d"

MAX_FIXED_SLOTS = 9


@immutable_class
class W_AbstractImmutable_PointersObject(W_PointersObject):
    """Abstract `W_PointersObject` subclass for immutable pointers objects."""
    _attrs_ = []
    _immutable_fields_ = []
    repr_classname = ('%s_AbstractImmutable' %
                      W_PointersObject.repr_classname)

    def __init__(self, space, w_cls, pointers_w):
        """
        Initialize immutable pointers object, but avoid initializing storage
        by calling `W_AbstractObjectWithIdentityHash.__init__(self)` instead of
        `W_PointersObject.__init__(self)`.
        Additionally, reuse `self.strategy` slot to store class shadow.

        """
        W_AbstractObjectWithIdentityHash.__init__(self)
        self.strategy = w_cls.as_class_get_shadow(space)

    def getclass(self, space):
        """:returns: Class from class shadow stored in `self.strategy` slot."""
        return self.strategy.w_self()

    def class_shadow(self, space):
        """:returns: Class shadow stored in `self.strategy` slot."""
        class_shadow = self.strategy
        assert isinstance(class_shadow, ClassShadow)
        return class_shadow

    def size(self):
        """:raises: NotImplementedError"""
        raise NotImplementedError('abstract base class')

    def fetch(self, space, n0):
        """:raises: NotImplementedError"""
        raise NotImplementedError('abstract base class')

    def clone(self, space):
        my_pointers = self.fetch_all(space)
        w_result = self.__class__(space, self.getclass(space), my_pointers)
        w_result.store_all(space, my_pointers)
        return w_result


class W_Immutable_PointersObject(W_AbstractImmutable_PointersObject):
    """`W_PointersObject` subclass with immutable storage of variable size."""
    _immutable_fields_ = ['_storage[*]']
    repr_classname = '%s_Immutable_N' % W_PointersObject.repr_classname
    erase, unerase = map(
        staticmethod, rerased.new_erasing_pair('storage_eraser'))

    def __init__(self, space, w_cls, pointers_w):
        W_AbstractImmutable_PointersObject.__init__(self, space, w_cls, pointers_w)
        self._storage = self.erase(pointers_w)

    def size(self):
        return len(self.unerase(self._storage))

    def fetch(self, space, n0):
        return self.unerase(self._storage)[n0]


def generate_fixed_immutable_subclass(n_storage):
    """
    Generate `W_PointersObject` subclass with immutable storage of fixed size.

    :param n_storage: Number of storage slots.
    :returns: Immutable `W_PointersObject` subclass with fixed slots.
    """
    storage_iter = unrolling_iterable(range(n_storage))
    cls_name = '%s_Immutable_%s' % (W_PointersObject.repr_classname, n_storage)

    class W_FixedImmutable_PointersObject(W_AbstractImmutable_PointersObject):
        """`W_PointersObject` subclass with immutable storage of fixed size."""
        _storages_ = [(STORAGE_ATTR_TEMPLATE % x) for x in storage_iter]
        _attrs_ = _storages_
        _immutable_fields_ = _storages_
        repr_classname = cls_name

        def __init__(self, space, w_cls, pointers_w):
            W_AbstractImmutable_PointersObject.__init__(self, space, w_cls, pointers_w)
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

POINTERS_CLASSES = []
for n_storage in range(0, MAX_FIXED_SLOTS + 1):
    POINTERS_CLASSES.append(generate_fixed_immutable_subclass(n_storage))
POINTERS_CLASSES.append(W_Immutable_PointersObject)
POINTERS_CLASS_ITER = unrolling_iterable(enumerate(POINTERS_CLASSES))


def select_immutable_pointers_class(storage):
    """
    Select immutable `W_PointersObject` subclass for a given pointers storage.
    If there is no immutable `W_PointersObject` subclass with the right fixed
    storage size, it returns the immutable subclass with variable storage size.

    :param storage: Pointers to store.
    :returns: Immutable `W_PointersObject` subclass.
    """
    length = len(storage)
    for i, cls in POINTERS_CLASS_ITER:
        if i == length:
            return cls
    return W_Immutable_PointersObject
