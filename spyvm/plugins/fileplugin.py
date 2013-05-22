from spyvm import model, error
from spyvm.plugins.plugin import Plugin
from spyvm.primitives import PrimitiveFailedError


FilePlugin = Plugin()


@FilePlugin.expose_primitive(unwrap_spec=[object])
def primitiveDirectoryDelimitor(interp, s_frame, w_rcvr):
    import os
    return interp.space.wrap_char(os.path.sep)
