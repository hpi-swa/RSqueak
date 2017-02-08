import os

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives import index1_0
from rsqueakvm.util.system import IS_WINDOWS, IS_SPHINX

if IS_WINDOWS and not IS_SPHINX:
    raise LookupError

UnixOSProcessPlugin = Plugin()


@UnixOSProcessPlugin.expose_primitive(unwrap_spec=[object, index1_0])
def primitiveEnvironmentAt(interp, s_frame, w_rcvr, index):
    env_strings = ['%s=%s' % (k, v) for k, v in os.environ.items()]
    if index < len(env_strings):
        return interp.space.wrap_string(env_strings[index])
    raise PrimitiveFailedError
