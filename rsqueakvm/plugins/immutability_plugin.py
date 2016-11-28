from rsqueakvm.model import *
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.error import PrimitiveFailedError

ImmutabilityPlugin = Plugin()


def _patch_w_object():
    from rsqueakvm.model.base import W_Object

    def freeze(self):
        pass
    W_Object.freeze = freeze

_patch_w_object()


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object])
def immutableCopy(interp, s_frame, w_recv):
    try:
        immutable_class = w_recv.__class__.immutable_class
    except AttributeError:
        raise PrimitiveFailedError

    if isinstance(w_recv, W_PointersObject):
        pointers = w_recv.fetch_all(interp.space)
        w_result = immutable_class(interp.space, w_recv.getclass(interp.space),
                                   len(pointers))
        w_result.store_all(interp.space, pointers)
        w_result.freeze()
        return w_result

    raise PrimitiveFailedError


@ImmutabilityPlugin.expose_primitive(unwrap_spec=[object])
def isImmutable(interp, s_frame, w_recv):
    if w_recv.is_immutable():
        return interp.space.w_true
    return interp.space.w_false
