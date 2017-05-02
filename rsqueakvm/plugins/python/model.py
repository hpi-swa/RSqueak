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

    def getforeignclass(self):
        return py_space.type(self.wp_object)

    def initialize_w_class(self):
        self.w_class = W_PythonObject(self.wp_object.getclass(py_space))

    def guess_classname(self):
        return self.getforeignclass().getname(py_space)

    def str_content(self):
        return str(self.wp_object)

    def is_same_object(self, other):
        return (isinstance(other, W_PythonObject) and
                other.wp_object is self.wp_object)

    def make_class_shadow(self, space):
        return PythonClassShadow(space, self.wp_object)


class PythonClassShadow(ForeignLanguageClassShadow):
    _attrs_ = ['wp_object']
    _immutable_fields_ = ['wp_object']

    def __init__(self, space, wp_object):
        self.wp_object = wp_object
        ForeignLanguageClassShadow.__init__(self, space)

    def method_exists(self, method_name):
        # import pdb; pdb.set_trace()
        wp_method_name = py_space.newtext(method_name)
        try:
            if py_space.getattr(self.wp_object, wp_method_name) is not None:
                return True
        except OperationError as operror:
            print operror.errorstr(py_space)
        except Exception as e:
            print 'Unable to create method %s: %s' % (method_name, e)
        return False
