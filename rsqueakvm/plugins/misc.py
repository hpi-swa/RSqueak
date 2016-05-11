from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.plugin import Plugin

from rpython.rlib.rarithmetic import r_uint


MiscPrimitivePlugin = Plugin()

def _bytesHashLoop(bytes, start):
    hash = start
    for byte in bytes:
        hash = hash + ord(byte)
        low = r_uint(hash & 16383)
        hash = (0x260D * low +
                (((0x260D * (hash >> 14) + (0x0065 * low))
                  & 16383) * 16384)) & r_uint(0x0FFFFFFF)
    return hash

@MiscPrimitivePlugin.expose_primitive(unwrap_spec=[object, object, r_uint])
def primitiveStringHash(interp, s_frame, w_rcvr, thestring, initialHash):
    if not isinstance(thestring, W_BytesObject):
        raise PrimitiveFailedError
    hash = r_uint(initialHash) & r_uint(0xFFFFFFF)
    bytes = thestring.getbytes()
    return interp.space.wrap_smallint_unsafe(_bytesHashLoop(bytes, hash))
