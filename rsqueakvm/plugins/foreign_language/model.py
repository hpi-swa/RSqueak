from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.storage_classes import ClassShadow


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


class ForeignLanguageClassShadow(ClassShadow):
    _attrs_ = ['wp_object', 'wp_class']
    _immutable_fields_ = ['wp_class']

    # Overrides

    def changed(self):
        pass  # Changes to foreign classes are not handled in Smalltalk

    def lookup(self, w_selector):
        w_method = self.make_method(w_selector)
        if w_method is not None:
            return w_method
        fallback = self.fallback_class().as_class_get_shadow(self.space)
        return fallback.lookup(w_selector)

    # Abstract methods

    def fallback_class(self):
        raise NotImplementedError

    def make_method(self, w_selector):
        raise NotImplementedError
