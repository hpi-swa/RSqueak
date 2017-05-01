from rsqueakvm import error
from rsqueakvm.util import system
from rsqueakvm.plugins import PluginRegistry

from rpython.rlib import jit
from rpython.rlib.objectmodel import not_rpython


class Plugin(object):

    def __init__(self):
        if self.__class__ is Plugin:
            raise ValueError('Forbidden to instantiate Plugin(), '
                             'instantiate subclass instead.')

        self.primitives = {}
        self.userdata = {}

        if self.is_enabled():
            self.setup()

        PluginRegistry.add(self)

    def name(self):
        return self.__class__.__name__

    def is_optional(self):
        return False

    def is_enabled(self):
        if self.name() in system.disabled_plugins:
            return False
        if self.is_optional():
            return (self.name() in system.optional_plugins or system.IS_SPHINX)
        return True  # enabled by default

    @not_rpython
    def setup(self):
        "Called when enabled during instantiation."
        pass

    @not_rpython
    def patch(self):
        "Called once in the beginning of `main.py` to patch interpreter."
        pass

    @staticmethod
    def startup(space, argv):
        "Called after image has been loaded and space has been set up."
        pass

    def call(self, name, interp, s_frame, argcount, w_method):
        func = self._find_prim(name)
        if not func:
            raise error.PrimitiveFailedError("Not implemented: ", name)
        else:
            return func(interp, s_frame, argcount, w_method)

    @jit.elidable
    def _find_prim(self, name):
        return self.primitives.get(name, None)

    @not_rpython
    def expose_primitive(self, wrap_func=None, **kwargs):
        if not self.is_enabled():
            return lambda x: x  # do not install primitives when disabled
        from rsqueakvm.primitives import wrap_primitive, unwrap_alternatives
        if not wrap_func:
            if kwargs.get('unwrap_specs', None):
                wrap_func = unwrap_alternatives
            else:
                wrap_func = wrap_primitive
        def decorator(func):
            wrapped = wrap_func(**kwargs)(func)
            wrapped.func_name = "wrap_prim_" + func.func_name
            self.primitives[func.func_name] = wrapped
            return func
        return decorator

    def _freeze_(self):
        return True
