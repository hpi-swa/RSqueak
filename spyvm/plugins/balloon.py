from spyvm import model_display, model
from spyvm.error import PrimitiveFailedError
from spyvm.storage import AbstractCachingShadow
from spyvm.plugins.plugin import SimulatedPlugin

from rpython.rlib import jit, objectmodel
from rpython.rlib.rarithmetic import r_uint, intmask


def prepare_send(w_name, interp, s_frame, argcount, w_method, w_rcvr):
    args_w = s_frame.pop_and_return_n(argcount)
    # w_rcvr left on stack
    s_frame.push(w_name)
    s_frame.push(interp.space.wrap_list_unroll_safe(args_w))
    return 2

BalloonPlugin = SimulatedPlugin('w_simulateBalloonPrimitive', prepare_send)
