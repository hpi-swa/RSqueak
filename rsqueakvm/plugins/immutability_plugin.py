from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model import *
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.storage_classes import BYTES, POINTERS, WORDS
from rsqueakvm.util.version import Version

ImmutabilityPlugin = Plugin()

WRITE_OPERATIONS_W_OBJECT = [
    # W_Object
    'atput0', 'store', 'store_all', 'setword' '_become', 'fillin',
    'fillin_weak', 'fillin_finalize',
    # W_PointersObject
    'pointers_become_one_way',
    # W_BytesObject / W_WordsObject
    'setchar', 'short_atput0', 'setwords', 'convert_to_bytes_layout',
    'setbytes', 'mutate'
]


def immutable_class(cls):
    def is_immutable(self):
        return True
    cls.is_immutable = is_immutable

    for method_name in WRITE_OPERATIONS_W_OBJECT:
        if hasattr(cls, method_name):
            def noop(self, *args):
                pass
            setattr(cls, method_name, noop)
    return cls


def _add_immutable_w_pointersobject_subclass():
    @immutable_class
    class W_PointersObject_Immutable(W_PointersObject):
        _attrs_ = ['storage', 'w_class', '_instsize']
        _immutable_fields_ = ['storage[*]', '_instsize']
        repr_classname = '%s_Immutable' % W_PointersObject.repr_classname

        def __init__(self, space, w_cls, pointers_w):
            W_AbstractObjectWithIdentityHash.__init__(self)
            self.w_class = w_cls
            self.storage = pointers_w
            self._instsize = self.w_class.as_class_get_shadow(space).instsize()

        def getclass(self, space):
            return self.w_class

        def size(self):
            return len(self.storage) - self.instsize()

        def instsize(self):
            return self._instsize

        def fetch(self, space, n0):
            return self.storage[n0]

    W_PointersObject.immutable_subclass = W_PointersObject_Immutable


def _add_immutable_w_bytesobject_subclass():
    @immutable_class
    class W_BytesObject_Immutable(W_BytesObject):
        _attrs_ = ['storage']
        _immutable_fields_ = ['storage']
        repr_classname = '%s_Immutable' % W_BytesObject.repr_classname

        def __init__(self, space, w_cls, bytes_w):
            W_AbstractObjectWithClassReference.__init__(self, space, w_cls)
            self.storage = bytes_w

        def _bytes(self):
            return self.storage

        def _version(self):
            return None

    W_BytesObject.immutable_subclass = W_BytesObject_Immutable


def _add_immutable_w_wordsobject_subclass():
    @immutable_class
    class W_WordsObject_Immutable(W_WordsObject):
        _attrs_ = ['storage']
        _immutable_fields_ = ['storage']
        repr_classname = '%s_Immutable' % W_WordsObject.repr_classname

        def __init__(self, space, w_cls, words_w):
            W_AbstractObjectWithClassReference.__init__(self, space, w_cls)
            self.storage = words_w

        def _words(self):
            return self.storage

    W_WordsObject.immutable_subclass = W_WordsObject_Immutable


def _patch_w_object():
    def is_immutable(self):
        return False
    W_Object.is_immutable = is_immutable

_patch_w_object()
_add_immutable_w_pointersobject_subclass()
_add_immutable_w_bytesobject_subclass()
_add_immutable_w_wordsobject_subclass()


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object, object])
def primitiveImmutableFrom(interp, s_frame, w_cls, w_obj):
    space = interp.space
    instance_kind = w_cls.as_class_get_shadow(space).get_instance_kind()

    if instance_kind == POINTERS:
        pointers = w_obj.fetch_all(space)
        return W_PointersObject.immutable_subclass(space, w_cls, pointers)
    elif instance_kind == BYTES and isinstance(w_obj, W_BytesObject):
        return W_BytesObject.immutable_subclass(space, w_cls, w_obj.bytes)
    elif instance_kind == WORDS and isinstance(w_obj, W_WordsObject):
        return W_WordsObject.immutable_subclass(space, w_cls, w_obj.words)

    raise PrimitiveFailedError


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object])
def primitiveIsImmutable(interp, s_frame, w_recv):
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false
