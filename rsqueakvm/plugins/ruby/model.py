from rsqueakvm.plugins.foreign_language.model import (
    W_ForeignLanguageObject, ForeignLanguageClassShadow)
from rsqueakvm.plugins.ruby.objspace import ruby_space


class W_RubyObject(W_ForeignLanguageObject):
    _attrs_ = ['wr_object']
    _immutable_fields_ = ['wr_object']
    repr_classname = 'W_RubyObject'

    def __init__(self, wr_object):
        W_ForeignLanguageObject.__init__(self)
        self.wr_object = wr_object

    def getforeignclass(self):
        return ruby_space.getclass(self.wr_object)

    def initialize_w_class(self):
        self.w_class = W_RubyObject(self.wr_object.getclass(ruby_space))

    def guess_classname(self):
        return self.getforeignclass().name

    def str_content(self):
        return ruby_space.str_w(ruby_space.send(self.wr_object, 'inspect'))

    def is_same_object(self, other):
        return (isinstance(other, W_RubyObject) and
                other.wr_object is self.wr_object)

    def make_class_shadow(self, space):
        return RubyClassShadow(space, self.getforeignclass())


class RubyClassShadow(ForeignLanguageClassShadow):
    _attrs_ = ['wr_class']
    _immutable_fields_ = ['wr_class']

    def __init__(self, space, wr_class):
        self.wr_class = wr_class
        self.name = wr_class.name
        ForeignLanguageClassShadow.__init__(self, space)

    def method_exists(self, method_name):
        return self.wr_class.find_method(ruby_space, method_name) is not None
