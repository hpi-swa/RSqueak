import locale
import time

from rpython.rlib import rlocale

from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.error import PrimitiveFailedError


LocalePlugin = Plugin()


@LocalePlugin.expose_primitive(unwrap_spec=[object])
def primitiveLanguage(interp, s_frame, w_rcvr):
    fullname = rlocale.setlocale(rlocale.LC_ALL, None)
    if "." in fullname and "_" in fullname:
        return interp.space.wrap_string(fullname.split(".")[0].split("_")[0])
    raise PrimitiveFailedError

@LocalePlugin.expose_primitive(unwrap_spec=[object])
def primitiveCountry(interp, s_frame, w_rcvr):
    fullname = rlocale.setlocale(rlocale.LC_ALL, None)
    if "." in fullname and "_" in fullname:
        return interp.space.wrap_string(fullname.split(".")[0].split("_")[1])
    raise PrimitiveFailedError
