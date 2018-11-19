import os

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives import index1_0
from rsqueakvm.util.system import IS_WINDOWS


class UnixOSProcessPlugin(Plugin):

    def is_enabled(self):
        return Plugin.is_enabled(self) and not IS_WINDOWS

plugin = UnixOSProcessPlugin()


@plugin.expose_primitive(unwrap_spec=[object, index1_0])
def primitiveEnvironmentAt(interp, s_frame, w_rcvr, index):
    env_strings = ['%s=%s' % (k, v) for k, v in os.environ.items()]
    if index < len(env_strings):
        return interp.space.wrap_string(env_strings[index])
    raise PrimitiveFailedError


class Win32OSProcessPlugin(Plugin):

    def is_enabled(self):
        return Plugin.is_enabled(self) and IS_WINDOWS

plugin = Win32OSProcessPlugin()


@plugin.expose_primitive(unwrap_spec=[object])
def primitiveGetEnvironmentStrings(interp, s_frame, w_rcvr):
    return interp.space.wrap_string(
        '\n'.join(['%s=%s' % (k, v) for k, v in os.environ.items()]))
