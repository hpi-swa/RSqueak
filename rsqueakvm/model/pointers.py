from rsqueakvm import constants, error
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.numeric import W_SmallInteger

from rpython.rlib import objectmodel, jit
from rpython.rlib.rarithmetic import intmask
from rpython.rlib.rstrategies import rstrategies as rstrat


class W_PointersObject(W_AbstractObjectWithIdentityHash):
    """Common object."""
    _attrs_ = ['strategy', '_storage']
    strategy = None
    repr_classname = "W_PointersObject"
    rstrat.make_accessors(strategy='strategy', storage='_storage')

    def __init__(self, space, w_class, size, weak=False):
        """Create new object with size = fixed + variable size."""
        W_AbstractObjectWithIdentityHash.__init__(self)
        self._initialize_storage(space, w_class, size, weak)

    def _initialize_storage(self, space, w_class, size, weak=False):
        storage_type = space.strategy_factory.empty_storage_type(self, size, weak)
        space.strategy_factory.set_initial_strategy(self, storage_type, w_class, size)

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        # Recursive fillin required to enable specialized storage strategies.
        for g_obj in g_self.pointers:
            g_obj.fillin(space)
        pointers = g_self.get_pointers()
        storage_type = space.strategy_factory.strategy_type_for(self, pointers, weak=False)  # do not fill in weak lists, yet
        space.strategy_factory.set_initial_strategy(self, storage_type,
                                                    g_self.get_class(),
                                                    len(pointers), pointers)

    def fillin_weak(self, space, g_self):
        assert g_self.isweak()  # when we get here, this is true
        pointers = self.fetch_all(space)
        storage_type = space.strategy_factory.strategy_type_for(self, pointers, weak=True)
        space.strategy_factory.switch_strategy(self, storage_type)

    def is_weak(self):
        from rsqueakvm.storage import WeakListStrategy
        return isinstance(self.strategy, WeakListStrategy)

    def elidable_strategy(self):
        if jit.isconstant(self):
            return self._pure_strategy()
        else:
            return self.strategy.promoted(self)

    @jit.elidable
    def _pure_strategy(self):
        return self.strategy

    def safe_getclass(self, space):
        return self.strategy.promoted(self).getclass()

    def getclass(self, space):
        return jit.promote(self.elidable_strategy().getclass())

    def is_class(self, space):
        from rsqueakvm.storage_classes import ClassShadow
        if isinstance(self.strategy, ClassShadow):
            return True
        w_Metaclass = space.w_Metaclass
        w_class = self.getclass(space)
        if w_Metaclass.is_same_object(w_class):
            return True
        else:
            return w_Metaclass.is_same_object(w_class.getclass(space))

    def change_class(self, space, w_class):
        old_strategy = self.strategy
        new_strategy = old_strategy.instantiate(self, w_class)
        self._set_strategy(new_strategy)
        old_strategy._convert_storage_to(self, new_strategy)
        new_strategy.strategy_switched(self)

    def guess_classname(self):
        class_shadow = self.class_shadow(self.getclass(None).space())
        return class_shadow.name

    def invariant(self):
        from rsqueakvm import storage_classes
        return (W_AbstractObjectWithIdentityHash.invariant(self) and
                isinstance(self.getclass(None).strategy, storage_classes.ClassShadow))

    def space(self):
        return self.elidable_strategy().space

    def __str__(self):
        if self.strategy.provides_getname:
            return self.strategy.getname()
        else:
            return W_AbstractObjectWithIdentityHash.__str__(self)

    def repr_content(self):
        strategy_info = "no strategy"
        name = ""
        strategy_info = self.strategy.__repr__()
        if self.strategy.provides_getname:
            name = " [%s]" % self.strategy.getname()
        return '(%s) len=%d%s' % (strategy_info, self.size(), name)

    def unwrap_char(self, space):
        w_class = self.getclass(space)
        if not w_class.is_same_object(space.w_Character):
            raise error.UnwrappingError("expected Character")
        w_ord = self.fetch(space, constants.CHARACTER_VALUE_INDEX)
        if not isinstance(w_ord, W_SmallInteger):
            raise error.UnwrappingError("expected SmallInteger from Character")
        return chr(w_ord.value)

    @jit.look_inside_iff(lambda self, space: (
        (not self.class_shadow(space).isvariable()) or
        jit.isconstant(self.size())))
    def unwrap_array(self, space):
        # Check that our argument has pointers format and the class:
        if not self.getclass(space).is_same_object(space.w_Array):
            raise error.UnwrappingError
        return [self.at0(space, i) for i in range(self.size())]

    @jit.look_inside_iff(lambda self, space: (
        (not self.class_shadow(space).isvariable()) or
        jit.isconstant(self.size())))
    def fetch_all(self, space):
        return [self.fetch(space, i) for i in range(self.size())]

    @jit.look_inside_iff(lambda self, space, collection: (
        (not self.class_shadow(space).isvariable()) or len(collection) < 64))
    def store_all(self, space, collection):
        # Be tolerant: copy over as many elements as possible, set rest to nil.
        # The size of the object cannot be changed in any case.
        # TODO use store_all() provided by strategy?
        my_length = self.size()
        incoming_length = min(my_length, len(collection))
        i = 0
        while i < incoming_length:
            self.store(space, i, collection[i])
            i = i+1
        while i < my_length:
            self.store(space, i, space.w_nil)
            i = i+1

    def at0(self, space, index0):
        # To test, at0 = in varsize part
        return self.fetch(space, index0 + self.instsize())

    def atput0(self, space, index0, w_value):
        # To test, at0 = in varsize part
        self.store(space, index0 + self.instsize(), w_value)

    def fetch(self, space, n0):
        return self.elidable_strategy().fetch(self, n0)

    def store(self, space, n0, w_value):
        return self.elidable_strategy().store(self, n0, w_value)

    def size(self):
        return self.elidable_strategy().size(self)

    def instsize(self):
        return self.class_shadow(self.space()).instsize()

    @objectmodel.specialize.arg(2)
    def as_special_get_shadow(self, space, TheClass):
        shadow = self.elidable_strategy()
        if not isinstance(shadow, TheClass):
            shadow = space.strategy_factory.switch_strategy(self, TheClass)
        assert isinstance(shadow, TheClass)
        return shadow

    def as_class_get_shadow(self, space):
        from rsqueakvm.storage_classes import ClassShadow
        return self.as_special_get_shadow(space, ClassShadow)

    def as_context_get_shadow(self, space):
        from rsqueakvm.storage_contexts import ContextPartShadow
        return self.as_special_get_shadow(space, ContextPartShadow)

    def as_methoddict_get_shadow(self, space):
        from rsqueakvm.storage_classes import MethodDictionaryShadow
        return self.as_special_get_shadow(space, MethodDictionaryShadow)

    def as_cached_object_get_shadow(self, space):
        from rsqueakvm.storage import CachedObjectShadow
        return self.as_special_get_shadow(space, CachedObjectShadow)

    def as_observed_get_shadow(self, space):
        from rsqueakvm.storage import ObserveeShadow
        return self.as_special_get_shadow(space, ObserveeShadow)

    def can_become(self, w_other):
        return type(w_other) is type(self)

    def _become(self, w_other):
        assert isinstance(w_other, W_PointersObject)
        # Make sure our class shadow is initialized, we will need it
        if self.getclass(None):
            self.class_shadow(self.getclass(None).space())
        if w_other.getclass(None):
            w_other.class_shadow(w_other.getclass(None).space())
        # Only one strategy will handle the become (or none of them).
        # The receivers strategy gets the first shot.
        # If it doesn't want to, let the w_other's strategy handle it.
        if self.strategy.handles_become():
            self.strategy.become(w_other)
        elif w_other.strategy.handles_become():
            w_other.strategy.become(self)
        self.strategy, w_other.strategy = w_other.strategy, self.strategy
        self._storage, w_other._storage = w_other._storage, self._storage
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def pointers_become_one_way(self, space, from_w, to_w):
        ptrs = self.fetch_all(space)
        ptridx = 0
        for i, w_from in enumerate(from_w):
            try:
                ptridx = ptrs.index(w_from)
            except ValueError:
                continue
            w_to = to_w[i]
            ptrs[ptridx] = w_to
            w_from.post_become_one_way(w_to)
        self.store_all(space, ptrs)

    def clone(self, space):
        my_pointers = self.fetch_all(space)
        w_result = W_PointersObject(space, self.getclass(space),
                                    len(my_pointers))
        w_result.store_all(space, my_pointers)
        return w_result


# when changing this constant, also change the read and __write__ methods in
# IntMapStorageNode in storage.py (or generalize those methods to compile with
# getattrs)
_NUMBER_OF_INT_FIELDS = 3
_NUMBER_OF_INLINE_FIELDS = 2
class W_FixedPointersObject(W_PointersObject):
    """My instances represent only those pointers objects that have a fixed number
    of fields and no variable sized parts"""
    _attrs_ = ['_field1', '_field2', '_intField1', '_intField2', '_intField3', '_intFields', '_size']
    _immutable_attrs_ = ['_intFields?', '_size?']

    def __init__(self, space, w_class, size, weak=False):
        self._init_inline_fields()
        self._size = size
        W_PointersObject.__init__(self, space, w_class, size, weak=False)

    def size(self):
        return self._size

    def instsize(self):
        return self._size

    def fillin(self, space, g_self):
        self._init_inline_fields()
        self._size = len(g_self.pointers)
        W_PointersObject.fillin(self, space, g_self)

    def _init_inline_fields(self):
        self._intField1 = 0
        self._intField2 = 0
        self._intField3 = 0
        self._intFields = None
        self._field1 = None
        self._field2 = None

    def _swap_inline_fields(self, w_other):
        self._intField1, w_other._intField1 = w_other._intField1, self._intField1
        self._intField2, w_other._intField2 = w_other._intField2, self._intField2
        self._intField3, w_other._intField3 = w_other._intField3, self._intField3
        self._intFields, w_other._intFields = w_other._intFields, self._intFields
        self._field1, w_other._field1 = w_other._field1, self._field1
        self._field2, w_other._field2 = w_other._field2, self._field2

    def _become(self, w_other):
        assert isinstance(w_other, W_FixedPointersObject)
        W_PointersObject._become(self, w_other)
        self._swap_inline_fields(w_other)
        self._size, w_other._size = w_other._size, self._size

    def clone(self, space):
        my_pointers = self.fetch_all(space)
        w_result = W_FixedPointersObject(space, self.getclass(space),
                                         len(my_pointers))
        w_result.store_all(space, my_pointers)
        return w_result
