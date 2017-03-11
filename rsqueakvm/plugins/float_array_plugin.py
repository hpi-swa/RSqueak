from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives import wordlist, index1_0

from rpython.rlib.longlong2float import uint2singlefloat, singlefloat2uint
from rpython.rlib.rarithmetic import r_singlefloat, r_uint
from rpython.rtyper.lltypesystem import rffi


class FloatArrayPlugin(Plugin):
    pass

plugin = FloatArrayPlugin()


@plugin.expose_primitive(unwrap_spec=[wordlist, index1_0])
def primitiveAt(interp, s_frame, words, index0):
    uintword = rffi.cast(rffi.UINT, words[index0])
    singlefloat = uint2singlefloat(uintword)
    doublefloat = rffi.cast(rffi.DOUBLE, singlefloat)
    try:
        return interp.space.wrap_float(float(doublefloat))
    except IndexError:
        raise PrimitiveFailedError


@plugin.expose_primitive(unwrap_spec=[wordlist, index1_0, object])
def primitiveAtPut(interp, s_frame, words, index0, w_float):
    value = interp.space.unwrap_float(w_float)
    try:
        words[index0] = r_uint(singlefloat2uint(r_singlefloat(value)))
    except IndexError:
        raise PrimitiveFailedError
    return w_float
