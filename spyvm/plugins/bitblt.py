from spyvm import model_display, model
from spyvm.error import PrimitiveFailedError
from spyvm.storage import AbstractCachingShadow
from spyvm.plugins.plugin import Plugin, SimulatedPlugin

from rpython.rlib import jit, objectmodel
from rpython.rlib.rarithmetic import r_uint, intmask


BitBltPlugin = Plugin()


def prepare_copy_bits(interp, s_frame, argcount, w_method, w_rcvr):
    if "copyBitsAgain" != w_method.lookup_selector:
        raise PrimitiveFailedError
    if argcount != 0:
        raise PrimitiveFailedError
    return 0
BitBltPlugin.expose_simulated_primitive(
    'primitiveCopyBits', 'w_copyBitsSimulated', prepare_copy_bits
)

def prepare_warp_bits(interp, s_frame, argcount, w_method, w_rcvr):
    if argcount != 2:
        raise PrimitiveFailedError
    return 2
BitBltPlugin.expose_simulated_primitive(
    'primitiveWarpBits', 'w_warpBitsSimulated', prepare_warp_bits
)
