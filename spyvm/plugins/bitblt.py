from spyvm import model_display, model
from spyvm.error import PrimitiveFailedError, MetaPrimFailed
from spyvm.storage import AbstractCachingShadow
from spyvm.plugins.plugin import Plugin

from rpython.rlib import jit, objectmodel
from rpython.rlib.rarithmetic import r_uint, intmask


BitBltPlugin = Plugin()


def send(interp, s_frame, w_selector, w_rcvr, w_method, argcount=0):
    from spyvm.interpreter import Return
    from spyvm.error import MethodNotFound

    w_arguments = s_frame.peek_n(argcount)
    s_fallback = w_method.create_frame(interp.space, w_rcvr, w_arguments)
    s_fallback._s_sender = s_frame

    try:
        s_frame._sendSelector(w_selector, argcount, interp, w_rcvr, w_rcvr.class_shadow(interp.space), s_fallback=s_fallback)

    except Return, ret:
        return True
    except MethodNotFound:
        return False
    except MetaPrimFailed, e:
        interp.stack_frame(s_fallback, None)
    return False


def image_side_copy_bits(interp, s_frame, w_rcvr, w_method):
    if interp.image.w_simulateCopyBits is not interp.space.w_nil:
        return send(interp, s_frame, interp.image.w_simulateCopyBits, w_rcvr, w_method)
    elif interp.image.w_copyBitsSimulated is not interp.space.w_nil:
        return send(interp, s_frame, interp.image.w_copyBitsSimulated, w_rcvr, w_method)
    else:
        raise PrimitiveFailedError("BitBlt not implemented in the VM")


@BitBltPlugin.expose_primitive(unwrap_spec=None, clean_stack=True, compiled_method=True)
def primitiveCopyBits(interp, s_frame, argcount, w_method):
    if argcount == 0:
        w_rcvr = s_frame.peek(0)
    else:
        assert argcount == 1
        w_rcvr = s_frame.peek(1)

    if not isinstance(w_rcvr, model.W_PointersObject) or w_rcvr.size() < 15:
        raise PrimitiveFailedError("BitBlt primitive not called in BitBlt object!")

    image_side_copy_bits(interp, s_frame, w_rcvr, w_method)
    return w_rcvr

@BitBltPlugin.expose_primitive(unwrap_spec=None, clean_stack=True, compiled_method=True)
def primitiveWarpBits(interp, s_frame, argcount, w_method):
    w_rcvr = s_frame.peek(argcount)
    if not isinstance(w_rcvr, model.W_PointersObject) or w_rcvr.size() < 15:
        raise PrimitiveFailedError("BitBlt primitive not called in BitBlt object!")
    if interp.image.w_warpBitsSimulated is not interp.space.w_nil:
        # TODO: what about return value of send?
        if not send(interp, s_frame, interp.image.w_warpBitsSimulated, w_rcvr, w_method, argcount=argcount):
            raise PrimitiveFailedError
    else:
        raise PrimitiveFailedError
    return w_rcvr
