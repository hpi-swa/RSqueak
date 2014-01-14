from rpython.rlib import jit

from spyvm import error
from spyvm.primitives import wrap_primitive


class Plugin(object):
    def __init__(self):
        self.prims = {}
        self.userdata = {}

    def call(self, name, interp, s_frame, argcount, s_method):
        func = self._find_prim(name)
        if not func:
            raise error.PrimitiveFailedError("Not implemented: %s" % name)
        else:
            return func(interp, s_frame, argcount, s_method)

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

    def _freeze_(self):
        return True
