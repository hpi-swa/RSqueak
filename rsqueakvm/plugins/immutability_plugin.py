from rsqueakvm.model import *
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.error import PrimitiveFailedError

ImmutabilityPlugin = Plugin()


def _add_immutable_subclass(cls, blacklist):
    class ImmutableSubclass(cls):
        _attrs_ = ['_immutable_storage']
        _immutable_fields_ = ['_immutable_storage[*]']

        def __init__(self, space, w_cls, pointers):
            cls.__init__(self, space, w_cls, len(pointers))
            self._immutable_storage = pointers

        def fetch(self, space, n0):
            return self._immutable_storage[n0];

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
def immutableFrom(interp, s_frame, w_cls, w_obj):
    # import pdb; pdb.set_trace()
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
def isImmutable(interp, s_frame, w_recv):
    # import pdb; pdb.set_trace()
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false
