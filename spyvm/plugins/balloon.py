from spyvm import model_display, model
from spyvm.error import PrimitiveFailedError
from spyvm.storage import AbstractCachingShadow
from spyvm.plugins.plugin import Plugin

from rpython.rlib import jit, objectmodel
from rpython.rlib.rarithmetic import r_uint, intmask


class BalloonPluginClass(Plugin):
    def simulate(self, w_name, name, interp, s_frame, argcount, w_method):
        if not interp.image.w_simulateBalloonPrimitive:
            raise PrimitiveFailedError("No BalloonSimulation found")

        from spyvm.interpreter import Return
        from spyvm.error import MethodNotFound

        args_w = s_frame.peek_n(argcount)
        w_rcvr = s_frame.peek(argcount)

        s_class = w_rcvr.class_shadow(interp.space)
        try:
            s_class.lookup(interp.image.w_simulateBalloonPrimitive)
        except MethodNotFound:
            raise PrimitiveFailedError

        s_frame.push(w_rcvr)
        s_frame.push(w_name)
        s_frame.push(interp.space.wrap_list_unroll_safe(args_w))
        try:
            s_frame._sendSelfSelector(interp.image.w_simulateBalloonPrimitive, 2, interp)
        except Return, ret:
            # must clean the stack, including the rcvr
            s_frame.pop_n(argcount + 1)
            s_frame.push(ret.value)
            return
        else:
            raise PrimitiveFailedError


BalloonPlugin = BalloonPluginClass()
