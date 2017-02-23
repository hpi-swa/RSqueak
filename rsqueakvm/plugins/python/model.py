from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.storage_classes import ClassShadow
from rsqueakvm.storage import AbstractCachingShadow
from rsqueakvm.primitives.constants import EXTERNAL_CALL
from rsqueakvm.model.compiled_methods import (
    W_PreSpurCompiledMethod, W_SpurCompiledMethod)
from rsqueakvm.plugins.python.global_state import (
    py_space, w_python_object_class, w_python_plugin_send)

from pypy.interpreter.baseobjspace import W_Root as WP_Root
from pypy.interpreter.error import OperationError

from rpython.rlib import objectmodel


class W_PythonObject(W_PointersObject):
    _attrs_ = ['wp_object', 's_class']
    _immutable_fields_ = ['wp_object', 's_class?']
    repr_classname = 'W_PythonObject'

    def __init__(self, wp_object):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self.wp_object = wp_object
        # self.w_pyID = None
        self.s_class = None

    def at0(self, space, index0):
        # import pdb; pdb.set_trace()
        return space.w_nil

    def atput0(self, space, index0, w_value):
        # import pdb; pdb.set_trace()
        pass

    def fetch(self, space, n0):
        # import pdb; pdb.set_trace()
        return space.w_nil

    def store(self, space, n0, w_value):
        # import pdb; pdb.set_trace()
        pass

    def safe_getclass(self, space):
        return W_PythonObject(self.wp_object.getclass(py_space))

    def getclass(self, space):
        return self.safe_getclass(space)

    def class_shadow(self, space):
        wp_class = py_space.type(self.wp_object)
        return PythonClassShadow(space, self.wp_object, wp_class)

    # @staticmethod
    # @jit.elidable
    # def pure_class_shadow(space, wp_class):
    #     return PythonClassShadowCache.setdefault(
    #         wp_class, PythonClassShadow(space, wp_class))

    def is_same_object(self, other):
        return (isinstance(other, W_PythonObject) and
                other.wp_object is self.wp_object)


class PythonClassShadow(ClassShadow):
    _attrs_ = ['wp_object', 'wp_class']
    _immutable_fields_ = ['wp_class']

    def __init__(self, space, wp_object, wp_class):
        assert isinstance(wp_class, WP_Root)
        self.wp_object = wp_object
        self.wp_class = wp_class
        self.name = wp_class.name
        AbstractCachingShadow.__init__(
            self, space, space.w_nil, 0, space.w_nil)

    def changed(self):
        pass  # Changes to Python classes are handled in Python land

    def lookup(self, w_selector):
        w_method = self.make_method(w_selector)
        # import pdb; pdb.set_trace()
        if w_method is not None:
            return w_method
        w_po = w_python_object_class.get()
        return w_po.as_class_get_shadow(self.space).lookup(w_selector)

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
        w_cm.header = 0
        w_cm._primitive = EXTERNAL_CALL
        w_cm.literalsize = 2
        w_cm.islarge = False
        w_cm._tempsize = 0
        w_cm.argsize = 0
        w_cm.bytes = []
        w_cm.literals = [
            w_python_plugin_send.get(),
            w_selector
        ]
        return w_cm
