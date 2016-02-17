from spyvm import model
from spyvm.error import PrimitiveFailedError
from spyvm.plugins.plugin import Plugin
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

@MiscPrimitivePlugin.expose_primitive(unwrap_spec=[object, r_uint])
def primitiveStringHash(interp, s_frame, w_rcvr, initialHash):
    if not isinstance(w_rcvr, model.W_BytesObject):
        raise PrimitiveFailedError
    hash = r_uint(initialHash) & r_uint(0xFFFFFFF)
    bytes = w_rcvr.getbytes()
    return interp.space.wrap_int(_bytesHashLoop(bytes, hash))
