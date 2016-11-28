from rsqueakvm.model import *
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.error import PrimitiveFailedError

ImmutabilityPlugin = Plugin()


def _add_immutable_subclass(cls, blacklist):
    class ImmutableSubclass(cls):
        _attrs_ = ['_frozen']
        _frozen = False

        def freeze(self):
            self._frozen = True

        def is_immutable(self):
            return self._frozen

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

    def freeze(self):
        pass
    W_Object.freeze = freeze

    _add_immutable_subclass(W_PointersObject,
                            ['atput0', 'store', 'store_all', '_become'])

_patch_w_objects()


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object, object])
def asImmutable(interp, s_frame, w_recv, w_cls):
    # import pdb; pdb.set_trace()
    try:
        immutable_class = w_recv.__class__.immutable_class
    except AttributeError:
        raise PrimitiveFailedError

    if isinstance(w_recv, W_PointersObject):
        pointers = w_recv.fetch_all(interp.space)
        w_result = immutable_class(interp.space, w_cls, len(pointers))
        w_result.store_all(interp.space, pointers)
        w_result.freeze()
        return w_result

    raise PrimitiveFailedError


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object])
def isImmutable(interp, s_frame, w_recv):
    # import pdb; pdb.set_trace()
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false
