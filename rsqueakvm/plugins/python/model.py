from rsqueakvm.plugins import python
from rsqueakvm.plugins.foreign_language.model import (
    W_ForeignLanguageObject, ForeignLanguageClassShadow)
from rsqueakvm.plugins.python import py_space
from rsqueakvm.primitives.constants import EXTERNAL_CALL
from rsqueakvm.model.compiled_methods import (
    W_PreSpurCompiledMethod, W_SpurCompiledMethod)
from rsqueakvm.storage import AbstractCachingShadow

from pypy.interpreter.error import OperationError

from rpython.rlib import objectmodel


class W_PythonObject(W_ForeignLanguageObject):
    _attrs_ = ['wp_object', 's_class']
    _immutable_fields_ = ['wp_object', 's_class?']
    repr_classname = 'W_PythonObject'

    def __init__(self, wp_object):
        W_ForeignLanguageObject.__init__(self)
        self.wp_object = wp_object
        # self.w_pyID = None
        self.s_class = None

    def getclass(self, space):
        return W_PythonObject(self.wp_object.getclass(py_space))

    def class_shadow(self, space):
        return PythonClassShadow(space, self.wp_object)

    # @staticmethod
    # @jit.elidable
    # def pure_class_shadow(space, wp_class):
    #     return PythonClassShadowCache.setdefault(
    #         wp_class, PythonClassShadow(space, wp_class))

    def is_same_object(self, other):
        return (isinstance(other, W_PythonObject) and
                other.wp_object is self.wp_object)


class PythonClassShadow(ForeignLanguageClassShadow):
    _attrs_ = ['wp_object', 'wp_class']
    _immutable_fields_ = ['wp_class']

    def __init__(self, space, wp_object):
        self.wp_object = wp_object
        wp_class = py_space.type(self.wp_object)
        self.wp_class = wp_class
        self.name = wp_class.name
        AbstractCachingShadow.__init__(
            self, space, space.w_nil, 0, space.w_nil)

    def fallback_class(self):
        return python.w_python_object_class.get()

    def make_method(self, w_selector):
        # import pdb; pdb.set_trace()
        methodname = self.space.unwrap_string(w_selector)
        idx = methodname.find(':')
        if idx > 0:
            methodname = methodname[0:idx]

        # import pdb; pdb.set_trace()

        py_attr = None
        # py_attr = True if methodname in ['pyID'] else None  # 'printString'
        try:
            if py_attr is None:
                # check instance vars and methods
                py_attr = py_space.getattr(self.wp_object,
                                           py_space.newtext(methodname))
            if py_attr is None:
                # check class vars and methods
                py_attr = py_space.getattr(self.wp_class,
                                           py_space.newtext(methodname))
        except OperationError:
            pass
        except Exception as e:
            print 'Unable to create method %s: %s' % (methodname, e)
            return None
        if py_attr is None:
            # check builtins
            if self.wp_class is py_space.type(py_space.builtin):
                try:
                    builtin_func = py_space.builtin.get(methodname)
                    if builtin_func is None:
                        return None
                except OperationError:
                    return None
            else:
                return None
        if self.space.is_spur.is_set():
            w_cm = objectmodel.instantiate(W_SpurCompiledMethod)
        else:
            w_cm = objectmodel.instantiate(W_PreSpurCompiledMethod)
        w_cm._primitive = EXTERNAL_CALL
        w_cm.literalsize = 2
        w_cm.islarge = False
        w_cm._tempsize = 0
        w_cm.argsize = 0
        w_cm.compiledin_class = python.w_python_class.get()
        w_cm.lookup_selector = 'fakePythonSend'
        w_cm.bytes = []
        w_cm.literals = [
            python.w_python_plugin_send.get(),
            w_selector
        ]
        return w_cm
