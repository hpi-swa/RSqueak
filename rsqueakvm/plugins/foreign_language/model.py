from rsqueakvm.error import Exit
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.compiled_methods import (
    W_PreSpurCompiledMethod, W_SpurCompiledMethod)
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.primitives.constants import EXTERNAL_CALL
from rsqueakvm.storage_classes import AbstractCachingShadow, ClassShadow
from rsqueakvm.util.cells import QuasiConstant

from rpython.rlib import objectmodel
from rpython.rlib.rstrategies.rstrategies import StrategyMetaclass


class W_ForeignLanguageObject(W_AbstractObjectWithIdentityHash):
    _attrs_ = []
    _immutable_fields_ = []
    repr_classname = 'W_ForeignLanguageObject'

    # W_AbstractObjectWithIdentityHash overrides

    def at0(self, space, index0):
        return space.w_nil

    def atput0(self, space, index0, w_value):
        pass

    def fetch(self, space, n0):
        return space.w_nil

    def store(self, space, n0, w_value):
        pass

    # Abstract methods

    def getclass(self, space):
        raise NotImplementedError

    def class_shadow(self, space):
        raise NotImplementedError

    def is_same_object(self, other):
        raise NotImplementedError


class ForeignLanguageClassShadowMeta(StrategyMetaclass):
    def __new__(cls, name, bases, attrs):
        # import pdb; pdb.set_trace()
        if name != 'ForeignLanguageClassShadow':
            attrs['w_plugin_send'] = QuasiConstant(None, cls=W_PointersObject)
            attrs['w_foreign_class'] = QuasiConstant(
                None, cls=W_PointersObject)
            attrs['w_foreign_object_class'] = QuasiConstant(
                None, cls=W_PointersObject)

        return type.__new__(cls, name, bases, attrs)


class ForeignLanguageClassShadow(ClassShadow):
    __metaclass__ = ForeignLanguageClassShadowMeta
    _attrs_ = []

    def __init__(self, space):
        AbstractCachingShadow.__init__(
            self, space, space.w_nil, 0, space.w_nil)

    @staticmethod
    def load_special_objects(cls, language_name, space):
        cls.w_plugin_send.set(space.wrap_list_unroll_safe([
            space.wrap_string('%sPlugin' % language_name),
            space.wrap_string('send')
        ]))

        foreign_class = space.smalltalk_at(language_name)
        if foreign_class is None:
            # disable plugin?
            raise Exit('%s class not found.' % language_name)
        cls.w_foreign_class.set(foreign_class)

        foreign_object_class = space.smalltalk_at('%sObject' % language_name)
        if foreign_object_class is None:
            raise Exit('%sObject class not found.' % language_name)
        cls.w_foreign_object_class.set(foreign_object_class)

    # Overrides

    def changed(self):
        pass  # Changes to foreign classes are not handled in Smalltalk

    def lookup(self, w_selector):
        if self.method_exists(w_selector):
            return self.make_method(w_selector)
        return self.w_foreign_object_class.get().as_class_get_shadow(
            self.space).lookup(w_selector)

    # Abstract methods

    def method_exists(self, w_selector):
        raise NotImplementedError

    # Helpers
    def make_method(self, w_selector):
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
        w_cm.compiledin_class = self.w_foreign_class.get()
        w_cm.bytes = []
        w_cm.literals = [
            self.w_plugin_send.get(),
            w_selector
        ]
        return w_cm
