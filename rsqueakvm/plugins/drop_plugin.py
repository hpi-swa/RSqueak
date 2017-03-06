import os

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.util.system import IS_WINDOWS


DropPlugin = Plugin()


@DropPlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveDropRequestFileName(interp, s_frame, w_rcvr, index):
    path = interp.space.display().get_dropped_filename()
    if path is None:
        raise PrimitiveFailedError
    else:
        return interp.space.wrap_string(path)


@DropPlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveDropRequestFileHandle(interp, s_frame, w_rcvr, index):
    path = interp.space.display().get_dropped_filename()
    if path is None:
        raise PrimitiveFailedError
    else:
        mode = os.O_RDONLY
        if not IS_WINDOWS:
            mode |= os.O_BINARY
    try:
        file_descriptor = os.open(path, mode, 0666)
    except OSError:
        raise PrimitiveFailedError
    return interp.space.wrap_int(file_descriptor)
