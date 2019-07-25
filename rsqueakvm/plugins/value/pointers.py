"""
W_PointersValue

.. data:: MAX_SLOTS
Number of subclasses with fixed number of slots to generate.

"""

from rsqueakvm.plugins.value import W_Value
from rsqueakvm.storage_classes import ClassShadow

from .small_list import inline_small_list

from rpython.rlib import rerased, objectmodel, jit
from rpython.rlib.unroll import unrolling_iterable

MAX_SLOTS = 9


@inline_small_list(sizemax=MAX_SLOTS, immutable=True, nonull=True,
                   attrname="_raw_storage", factoryname="make_basic",
                   listgettername="_get_raw_storage",
                   listsizename="_get_raw_storage_width",
                   gettername="_get_raw_storage_at",
                   settername="__unsafe_set_raw_storage_at__")
class W_PointersValue(W_Value):
    _attrs_ = _immutable_fields_ = ['_shape']
    repr_classname = 'W_PointersValue'

    @staticmethod
    def make(objects_w, space, w_class):
        from .shape import get_object_tag
        tag = get_object_tag(space, w_class, len(objects_w))
        (shape, storage) = tag.default_shape.fusion(objects_w)
        return W_PointersValue.make_basic(storage, space, shape)
        
        
            
    def __init__(self, space, shape):
        W_Value.__init__(self)
        from .shape import CompoundShape
        assert isinstance(shape, CompoundShape)
        self._shape = shape

    def shape(self):
        return jit.promote(self._shape)

    def get_tag(self):
        return self.shape()._tag

    # shape api
    def get_children(self):
        return self.shape().get_children(self)
    # rsqueak api
    def fetch_all(self, space):
        return self.get_children()


    # shape api
    def get_child(self, index):
        return self.shape().get_child(self, index)
    # rsqueak api
    def fetch(self, space, n0):
        return self.get_child(n0)
    # squeak api
    def at0(self, space, index0):
        # To test, at0 = in varsize part
        return self.fetch(space, index0 + self.instsize())
    
    # shape api
    def get_number_of_children(self):
        return self.shape().get_number_of_direct_children()
    # rsqueak api
    size = get_number_of_children

    def instsize(self):
        return self.class_shadow(None).instsize()


    def _tag(self):
        return self.shape()._tag

    def getclass(self, space):
        return self._tag().getclass()
    
    def class_shadow(self, space):
        class_shadow = self._tag().class_shadow()
        assert isinstance(class_shadow, ClassShadow)
        return class_shadow
    
    def clone(self, space):
        (shape, storage) = self._tag().default_shape.fusion(self.get_children())
        return W_PointersValue.make_basic(storage, space, shape)

    # stubs to catch WONTFIX cases
    def fillin(self, space, g_self):
        assert False, "cannot fillin"
    def fillin_weak(self, space, g_self):
        assert False, "cannot fillin_weak"
    def change_class(self, space, w_class):
        assert False, "cannot change my class"
    def _become(self, w_other):
        assert False, "to become or not to become. the latter, tho"
    def pointers_become_one_way(self, space, from_w, to_w):
        assert False, "to become or not to become. the latter, tho"

    def space(self):
        assert False, "where should I get that from?"
        

    # non-supported cases
    def is_weak(self):
        return False

    def safe_getclass(self, space):
        # there's no such thing here..
        return self.getclass(space)

    def guess_classname(self):
        return self._tag().name

    # we do not unwrap most stuff
    def unwrap_char(self, space):
        from rsqueakvm.error import UnwrappingError
        raise UnwrappingError
    def unwrap_array(self, space):
        from rsqueakvm.error import UnwrappingError
        raise UnwrappingError
