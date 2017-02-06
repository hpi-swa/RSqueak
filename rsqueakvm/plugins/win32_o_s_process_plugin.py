import os

from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.util.system import IS_WINDOWS, IS_SPHINX

if not IS_WINDOWS and not IS_SPHINX:
    raise LookupError


Win32OSProcessPlugin = Plugin()


@Win32OSProcessPlugin.expose_primitive(unwrap_spec=[object])
def primitiveGetEnvironmentStrings(interp, s_frame, w_rcvr):
    return interp.space.wrap_string(
        '\n'.join(['%s=%s' % (k, v) for k, v in os.environ.items()]))
