from rpython.rlib import jit

from spyvm import error


class Plugin(object):
    def __init__(self):
        self.prims = {}
        self.userdata = {}

    def call(self, name, interp, s_frame, argcount, w_method):
        func = self._find_prim(name)
        if not func:
            raise error.PrimitiveFailedError("Not implemented: ", name)
        else:
            return func(interp, s_frame, argcount, w_method)

    @jit.elidable
    def _find_prim(self, name):
        return self.prims.get(name, None)

    def expose_primitive(self,  wrap_func=None, **kwargs):
        from spyvm.primitives import wrap_primitive, unwrap_alternatives
        if not wrap_func:
            if kwargs.get('unwrap_specs', None):
                wrap_func = unwrap_alternatives
            else:
                wrap_func = wrap_primitive
        def decorator(func):
            wrapped = wrap_func(**kwargs)(func)
            wrapped.func_name = "wrap_prim_" + func.func_name
            self.prims[func.func_name] = wrapped
            return func
        return decorator

    def _freeze_(self):
        return True
