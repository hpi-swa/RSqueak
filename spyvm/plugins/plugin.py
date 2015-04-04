from rpython.rlib import jit

from spyvm import error
from spyvm.primitives import wrap_primitive
from spyvm.error import PrimitiveFailedError


# Use this for plugins implemented in RPython
class Plugin(object):
    def __init__(self):
        self.prims = {}
        self.userdata = {}

    def call(self, w_name, interp, s_frame, argcount, w_method):
        name = jit.promote(w_name.as_string())
        func = self._find_prim(name)
        if not func:
            raise error.PrimitiveFailedError("Not implemented: %s" % name)
        else:
            return func(interp, s_frame, argcount, w_method)

    @jit.elidable
    def _find_prim(self, name):
        return self.prims.get(name, None)

    def expose_primitive(self, unwrap_spec=None, no_result=False,
                         result_is_new_frame=False, clean_stack=True,
                         compiled_method=False):
        def decorator(func):
            wrapped = wrap_primitive(
                unwrap_spec=unwrap_spec, no_result=no_result,
                result_is_new_frame=result_is_new_frame,
                clean_stack=clean_stack, compiled_method=compiled_method
            )(func)
            wrapped.func_name = "wrap_prim_" + func.func_name
            self.prims[func.func_name] = wrapped
            return func
        return decorator

    def expose_simulated_primitive(self, name, selector, prep_code):
        def wrapped_prep_code(ignored, interp, s_frame, argcount, w_method, w_rcvr):
            return prep_code(interp, s_frame, argcount, w_method, w_rcvr)
        caller = SimulatedPlugin(selector, wrapped_prep_code)
        def func(interp, s_frame, argcount, w_method):
            caller.call(None, interp, s_frame, argcount, w_method)
        func.func_name = name
        return self.expose_primitive(unwrap_spec=None, no_result=True,
                                     result_is_new_frame=False, clean_stack=False,
                                     compiled_method=True)(func)

    def _freeze_(self):
        return True


# Use this for Plugins that call simulation code in the image, e.g. from VMMaker
def SimulatedPlugin(attr, prepare_send):
    getselector = "\n".join(["def getselector(self, interp):"
                             "    return interp.image." + attr])
    class SimPrimClass():
        exec getselector

        def call(self, w_name, interp, s_frame, argcount, w_method):
            w_sel = self.getselector(interp)
            if not w_sel or w_sel is interp.space.w_nil:
                raise PrimitiveFailedError("Missing simulation selector")

            from spyvm.interpreter import Return
            from spyvm.error import MethodNotFound

            w_rcvr = s_frame.peek(argcount)
            s_class = w_rcvr.class_shadow(interp.space)
            try:
                s_class.lookup(w_sel)
            except MethodNotFound:
                raise PrimitiveFailedError("Simulation called on wrong rcvr")

            sim_argc = prepare_send(w_name, interp, s_frame, argcount, w_method, w_rcvr)
            s_frame._sendSelfSelector(w_sel, sim_argc, interp)

        def _freeze_(self):
            return True

    return SimPrimClass()
