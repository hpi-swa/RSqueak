from rsqueakvm.model import *
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.error import PrimitiveFailedError

ImmutabilityPlugin = Plugin()


def _add_immutable_subclass(cls, blacklist):
    class ImmutableSubclass(cls):
        _attrs_ = ['storage', 'w_class', '_instsize']
        _immutable_fields_ = ['storage[*]', '_instsize']

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

        def is_immutable(self):
            return True

    # Decorate blacklisted methods with immutability check
    def restrict_access(fn):
        def decorator(self, *args):
            if not self.is_immutable():
                fn(self, *args)
        return decorator
    for method_name in blacklist:
        method = getattr(ImmutableSubclass, method_name)
        if hasattr(ImmutableSubclass, method_name):
            setattr(ImmutableSubclass, method_name, restrict_access(method))

    ImmutableSubclass.__name__ = '%s_Immutable' % cls.__name__
    ImmutableSubclass.repr_classname = '%s_Immutable' % cls.repr_classname

    cls.immutable_class = ImmutableSubclass


def _patch_w_objects():
    from rsqueakvm.model.base import W_Object
    from rsqueakvm.model.pointers import W_PointersObject

    def is_immutable(self):
        return False
    W_Object.is_immutable = is_immutable

    _add_immutable_subclass(W_PointersObject,
                            ['atput0', 'store', 'store_all', '_become'])

_patch_w_objects()


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object, object])
def primitiveImmutableFrom(interp, s_frame, w_cls, w_obj):
    try:
        immutable_subclass = w_cls.__class__.immutable_class
    except AttributeError:
        raise PrimitiveFailedError

    if isinstance(w_cls, W_PointersObject):
        pointers = w_obj.fetch_all(interp.space)
        w_result = immutable_subclass(interp.space, w_cls, pointers)
        return w_result

    raise PrimitiveFailedError


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object])
def primitiveIsImmutable(interp, s_frame, w_recv):
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false
