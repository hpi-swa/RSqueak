from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model import *
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.storage_classes import BYTES, POINTERS, WORDS, ClassShadow

from rpython.rlib import jit, rerased
from rpython.rlib.unroll import unrolling_iterable

"""
Documentation goes here.
"""


ImmutabilityPlugin = Plugin()

WRITE_OPERATIONS = [
    # W_Object
    'atput0', 'store', 'store_all', 'setword' '_become', 'fillin',
    'fillin_weak', 'fillin_finalize',
    # W_PointersObject
    'pointers_become_one_way',
    # W_BytesObject / W_WordsObject
    'setchar', 'short_atput0', 'setwords', 'convert_to_bytes_layout',
    'setbytes', 'mutate'
]
STORAGE_ATTR_TEMPLATE = "storage_%d"


def immutable_class(cls):
    def is_immutable(self):
        return True
    cls.is_immutable = is_immutable

    for method_name in WRITE_OPERATIONS:
        if hasattr(cls, method_name):
            def noop(self, *args):
                pass
            setattr(cls, method_name, noop)
    return cls


@immutable_class
class W_PointersObject_AbstractImmutable(W_PointersObject):
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


def _generate_fixed_w_pointersobject_class(n_storage):

    storage_iter = unrolling_iterable(range(n_storage))
    cls_name = '%s_Immutable_%s' % (W_PointersObject.repr_classname, n_storage)

    class W_PointersObject_Immutable_Fixed(W_PointersObject_AbstractImmutable):
        _storages_ = [(STORAGE_ATTR_TEMPLATE % x) for x in storage_iter]
        _attrs_ = _storages_
        _immutable_fields_ = _storages_
        repr_classname = cls_name

        def __init__(self, space, w_cls, pointers_w):
            W_PointersObject_AbstractImmutable.__init__(self, space, w_cls)
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

    W_PointersObject_Immutable_Fixed.__name__ = cls_name

    return W_PointersObject_Immutable_Fixed


def _immutable_w_pointersobject_subclass():
    class W_PointersObject_Immutable_N(W_PointersObject_AbstractImmutable):
        _immutable_fields_ = ['_storage[*]']
        repr_classname = '%s_Immutable_N' % W_PointersObject.repr_classname
        erase, unerase = rerased.new_erasing_pair('storage_eraser')

        def __init__(self, space, w_cls, pointers_w):
            W_PointersObject_AbstractImmutable.__init__(self, space, w_cls)
            self._storage = self.erase(pointers_w)

        def size(self):
            return len(self.unerase(self._storage))

        def fetch(self, space, n0):
            return self.unerase(self._storage)[n0]

    classes = []
    for n_storage in range(0, 10):
        classes.append(_generate_fixed_w_pointersobject_class(n_storage))
    classes.append(W_PointersObject_Immutable_N)
    return classes


def _add_immutable_w_bytesobject_subclass():
    @immutable_class
    class W_BytesObject_Immutable(W_BytesObject):
        repr_classname = '%s_Immutable' % W_BytesObject.repr_classname

        def __init__(self, space, w_cls, bytes_w):
            W_AbstractObjectWithClassReference.__init__(self, space, w_cls)
            self.bytes = bytes_w

        @jit.elidable
        def _bytes(self):
            return W_BytesObject._bytes(self)

        def _version(self):
            return None

    W_BytesObject.immutable_subclass = W_BytesObject_Immutable


def _add_immutable_w_wordsobject_subclass():
    @immutable_class
    class W_WordsObject_Immutable(W_WordsObject):
        repr_classname = '%s_Immutable' % W_WordsObject.repr_classname

        def __init__(self, space, w_cls, words_w):
            W_AbstractObjectWithClassReference.__init__(self, space, w_cls)
            self.words = words_w

        @jit.elidable
        def _words(self):
            return W_WordsObject._words(self)

    W_WordsObject.immutable_subclass = W_WordsObject_Immutable


def _patch_w_object():
    def is_immutable(self):
        return False
    W_Object.is_immutable = is_immutable

_patch_w_object()

pointers_classes = _immutable_w_pointersobject_subclass()
pointers_class_iter = unrolling_iterable(enumerate(pointers_classes))


def select_pointers_class(storage):
    length = len(storage)
    for i, cls in pointers_class_iter:
        if i == length:
            return cls
    # otherwise:
    return pointers_classes[-1]


_add_immutable_w_bytesobject_subclass()
_add_immutable_w_wordsobject_subclass()


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
        cls = select_pointers_class(pointers)
        return cls(space, w_cls, pointers)
    elif instance_kind == BYTES and isinstance(w_obj, W_BytesObject):
        return W_BytesObject.immutable_subclass(space, w_cls, w_obj.bytes)
    elif instance_kind == WORDS and isinstance(w_obj, W_WordsObject):
        return W_WordsObject.immutable_subclass(space, w_cls, w_obj.words)

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
        cls = select_pointers_class(w_arguments)
        return cls(space, w_cls, w_arguments)
    # TBD:
    # elif instance_kind == BYTES and isinstance(w_obj, W_BytesObject):
    #     return W_BytesObject.immutable_subclass(space, w_cls, w_obj.bytes)
    # elif instance_kind == WORDS and isinstance(w_obj, W_WordsObject):
    #     return W_WordsObject.immutable_subclass(space, w_cls, w_obj.words)

    raise PrimitiveFailedError


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
