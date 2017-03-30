from rsqueakvm.plugins.foreign_language.model import (
    W_ForeignLanguageObject, ForeignLanguageClassShadow)
from rsqueakvm.plugins.python.objspace import py_space

from pypy.interpreter.error import OperationError


class W_PythonObject(W_ForeignLanguageObject):
    _attrs_ = ['wp_object']
    _immutable_fields_ = ['wp_object']
    repr_classname = 'W_PythonObject'

    def __init__(self, wp_object):
        W_ForeignLanguageObject.__init__(self)
        self.wp_object = wp_object

    def getclass(self, space):
        return W_PythonObject(self.wp_object.getclass(py_space))

    def getforeignclass(self, space):
        return py_space.type(self.wp_object)

    def is_same_object(self, other):
        return (isinstance(other, W_PythonObject) and
                other.wp_object is self.wp_object)

    def make_class_shadow(self, space):
        return PythonClassShadow(
            space, self.getforeignclass(space), self.wp_object)


class PythonClassShadow(ForeignLanguageClassShadow):
    _attrs_ = ['wp_object']
    _immutable_fields_ = []

    def __init__(self, space, wp_class, wp_object):
        self.wp_object = wp_object
        self.name = wp_class.name
        ForeignLanguageClassShadow.__init__(self, space)

    def method_exists(self, w_selector):
        # import pdb; pdb.set_trace()
        methodname = self.space.unwrap_string(w_selector)
        idx = methodname.find(':')
        if idx > 0:
            methodname = methodname[0:idx]
        wp_methodname = py_space.newtext(methodname)
        try:
            if py_space.getattr(self.wp_object, wp_methodname) is not None:
                return True
        except OperationError as operror:
            print operror.errorstr(py_space)
        except Exception as e:
            print 'Unable to create method %s: %s' % (methodname, e)
        return False
