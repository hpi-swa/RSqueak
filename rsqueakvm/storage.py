
import weakref
import sys

from rsqueakvm import constants
from rsqueakvm.model.character import W_Character
from rsqueakvm.model.numeric import W_Float, W_SmallInteger, W_MutableSmallInteger
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.util.version import VersionMixin, elidable_for_version

from rpython.rlib import jit
from rpython.rlib.objectmodel import import_from_mixin
from rpython.rlib.rstrategies import rstrategies as rstrat

"""
A note on terminology:
A normal smalltalk objects contains references to other objects. These pointers
are the *storage* of the object. In RSqueak, each W_PointersObject passes
handling of its storage to a *strategy* object (subclass of AbstractStrategy).
Most objects are handled by a subclass of SimpleStorageStrategy. These classes
implement the actual "storage strategy" concept ("Storage Strategies for Collections
in Dynamically Typed Languages", Bolcz et al., 2013). To support these simple
strategies, W_PointersObject have a _storage field for arbitrary usage by their
strategy. These strategies are singleton for their class.
Strategies including the "ShadowMixin" are called *shadows*. Shadows are not
singletons and are bound to a single W_PointersObject and store additional state
required to implement the language semantics. ShadowMixin is used by ContextPartShadow
(storage_context.py) and AbstractGenericStrategy, which is extended by various classes
in storage_classes.py.
"""

class AbstractStrategy(object):
    """Subclasses of this handle the information contained in Smalltalk objects.
    The common API allows to store and fetch elements from object slots.
    Every object has some kind of storage representation attached.
    Some subclasses (those with *Shadow in their name) contain additional information,
    required by the VM. These 'shadows' not only manage the memory of their Smalltalk objects,
    but are also the VM-internal representation of these objects.
    """
    _attrs_ = ['space', 'w_class']
    _immutable_fields_ = ['space', 'w_class']
    provides_getname = False
    repr_classname = "AbstractStrategy"
    __metaclass__ = rstrat.StrategyMetaclass
    import_from_mixin(rstrat.AbstractStrategy)

    def __init__(self, space, w_self, size, w_class):
        self.space = space
        self.w_class = w_class
    def getname(self):
        raise NotImplementedError("Abstract class")
    def __repr__(self):
        if self.provides_getname:
            return "<%s %s>" % (self.repr_classname, self.getname())
        else:
            return "<%s>" % self.repr_classname
    def strategy_factory(self):
        return self.space.strategy_factory
    def _convert_storage_from_AllNilStrategy(self, w_self, all_nil_storage):
        # Fields are initialized to nil
        self._initialize_storage(w_self, all_nil_storage.size(w_self))
    def insert(self, w_self, index0, list_w):
        raise NotImplementedError("Smalltalk objects are fixed size")
    def delete(self, w_self, start, end):
        raise NotImplementedError("Smalltalk objects are fixed size")
    def is_shadow(self):
        return False
    def handles_become(self):
        """Only shadows are non-singletons and actually handle become"""
        return self.is_shadow()
    def become(self, w_other):
        raise NotImplementedError("This strategy doesn't handle become.")
    def onesided_become(self, w_other):
        # This is only needed in ShadowMixin, but has to be pulled up here because
        # both AbstractGenericShadow and ContextPartShadow use it.
        raise NotImplementedError("This strategy doesn't handle become.")
    def getclass(self):
        return self.w_class
    def instantiate(self, w_self, w_class):
        if self._is_singleton:
            new_strategy = self.strategy_factory().strategy_singleton_instance(self.instantiate_type, w_class)
        else:
            size = self.size(w_self)
            new_strategy = self.strategy_factory().instantiate_strategy(self.instantiate_type, w_class, w_self, size)
        return new_strategy

    def promote_if_neccessary(self):
        return jit.promote(self)

# ========== Storage classes implementing storage strategies ==========

class SimpleStorageStrategy(AbstractStrategy):
    """
    Singleton strategies handle 'simple' object storage in normal objects, without
    additional VM-internal information.
    Depending on the data inside an object, different optimizing strategies are used.
    """
    repr_classname = "SimpleStorageStrategy"
    _attrs_ = []
    import_from_mixin(rstrat.UnsafeIndexingMixin)

    def default_value(self):
        return self.space.w_nil

@rstrat.strategy()
class ListStrategy(SimpleStorageStrategy):
    _attrs_ = []
    repr_classname = "ListStrategy"
    import_from_mixin(rstrat.GenericStrategy)

    def _wrap(self, w_value):
        if isinstance(w_value, W_SmallInteger):
            assert isinstance(w_value, W_MutableSmallInteger)
            return self.space.wrap_smallint_unsafe(w_value.value)
        else:
            return w_value

    def _unwrap(self, w_value):
        if isinstance(w_value, W_SmallInteger):
            return W_MutableSmallInteger(w_value.value)
        else:
            return w_value

    def store(self, w_self, index0, w_value):
        self.check_index_store(w_self, index0)
        storage = self.get_storage(w_self)
        if isinstance(w_value, W_SmallInteger):
            w_old = storage[index0]
            if isinstance(w_old, W_SmallInteger):
                assert isinstance(w_old, W_MutableSmallInteger)
                w_old.set_value(w_value.value)
            else:
                storage[index0] = W_MutableSmallInteger(w_value.value)
        else:
            storage[index0] = w_value

ListStrategy.instantiate_type = ListStrategy


class ListEntry(object):
    _attrs_ = ['strong_content', 'weak_content']
    _immutable_fields_ = ['strong_content', 'weak_content']

    @staticmethod
    def build(value, is_instvar):
        # Strong references to:
        #  - instance variables
        #  - SmallIntegers (they used to be tagged in the reference implementation)
        #  - symbols (they lived forever in the reference implementation)
        if ListEntry.is_strong_anyway(value, is_instvar):
            return StrongListEntry(value)
        else:
            return WeakListEntry(value)

    @staticmethod
    def is_strong_anyway(value, is_instvar):
        return (is_instvar or
                isinstance(value, W_SmallInteger) or
                isinstance(value, W_BytesObject) or
                isinstance(value, W_CompiledMethod) or
                isinstance(value, W_Character) or
                isinstance(value, W_Float))

class StrongListEntry(ListEntry):
    def __init__(self, value):
        self.strong_content = value
        self.weak_content = None

    def get(self):
        return self.strong_content

class WeakListEntry(ListEntry):
    _attrs_ = ['strong_content', 'weak_content']
    _immutable_fields_ = ['strong_content', 'weak_content']
    def __init__(self, value):
        self.strong_content = None
        self.weak_content = weakref.ref(value)

    def get(self):
        return self.weak_content()

@rstrat.strategy()
class WeakListStrategy(SimpleStorageStrategy):
    repr_classname = "WeakListStrategy"
    #import_from_mixin(rstrat.WeakGenericStrategy)
    import_from_mixin(rstrat.StrategyWithStorage)

    def _wrap(self, value):
        assert isinstance(value, ListEntry)
        return value.get() or self.default_value()

    def _unwrap(self, value, index, w_self):
        assert value is not None
        return ListEntry.build(value, index < w_self.instsize())

    def _check_can_handle(self, value):
        return True

    def _initialize_storage(self, w_self, initial_size):
        default = StrongListEntry(self.default_value())
        self.set_storage(w_self, [default] * initial_size)

    @jit.unroll_safe
    def _convert_storage_from(self, w_self, previous_strategy):
        size = previous_strategy.size(w_self)
        new_storage = [ self._unwrap(previous_strategy.fetch(w_self, i), i, w_self)
                        for i in range(size) ]
        self.set_storage(w_self, new_storage)

    def store(self, w_self, index0, wrapped_value):
        self.check_index_store(w_self, index0)
        if self._check_can_handle(wrapped_value):
            unwrapped = self._unwrap(wrapped_value, index0, w_self)
            self.get_storage(w_self)[index0] = unwrapped
        else:
            self._cannot_handle_store(w_self, index0, wrapped_value)

    @jit.unroll_safe
    def insert(self, w_self, start, list_w):
        # This is following Python's behaviour - insert automatically
        # happens at the beginning of an array, even if index is larger
        if start > self.size(w_self):
            start = self.size(w_self)
        for i in range(len(list_w)):
            if self._check_can_handle(list_w[i]):
                self.get_storage(w_self).insert(start + i, self._unwrap(list_w[i], start + i, w_self))
            else:
                self._cannot_handle_insert(w_self, start + i, list_w[i:])
                return

    def delete(self, w_self, start, end):
        self.check_index_range(w_self, start, end)
        assert start >= 0 and end >= 0
        del self.get_storage(w_self)[start : end]
WeakListStrategy.instantiate_type = WeakListStrategy

@rstrat.strategy(generalize=[ListStrategy])
class SmallIntegerOrNilStrategy(SimpleStorageStrategy):
    repr_classname = "SmallIntegerOrNilStrategy"
    import_from_mixin(rstrat.TaggingStrategy)
    contained_type = W_SmallInteger
    def wrap(self, val): return self.space.wrap_smallint_unsafe(val)
    def unwrap(self, w_val): return self.space.unwrap_int(w_val)
    def wrapped_tagged_value(self): return self.space.w_nil
    def unwrapped_tagged_value(self): return constants.MAXINT
SmallIntegerOrNilStrategy.instantiate_type = SmallIntegerOrNilStrategy

@rstrat.strategy(generalize=[ListStrategy])
class CharacterOrNilStrategy(SimpleStorageStrategy):
    repr_classname = "CharacterOrNilStrategy"
    import_from_mixin(rstrat.TaggingStrategy)
    contained_type = W_Character
    def wrap(self, val): return W_Character(val)
    def unwrap(self, w_val):
        # XXX why would you think, this could be a W_Object?
        assert isinstance(w_val, W_Character)
        return w_val.value
    def wrapped_tagged_value(self): return self.space.w_nil
    def unwrapped_tagged_value(self): return constants.MAXINT
CharacterOrNilStrategy.instantiate_type = CharacterOrNilStrategy

@rstrat.strategy(generalize=[ListStrategy])
class FloatOrNilStrategy(SimpleStorageStrategy):
    repr_classname = "FloatOrNilStrategy"
    import_from_mixin(rstrat.TaggingStrategy)
    contained_type = W_Float
    tag_float = sys.float_info.max
    def wrap(self, val): return self.space.wrap_float(val)
    def unwrap(self, w_val): return self.space.unwrap_float(w_val)
    def wrapped_tagged_value(self): return self.space.w_nil
    def unwrapped_tagged_value(self): return self.tag_float
FloatOrNilStrategy.instantiate_type = FloatOrNilStrategy

@rstrat.strategy(generalize=[
    SmallIntegerOrNilStrategy,
    FloatOrNilStrategy,
    ListStrategy])
class AllNilStrategy(SimpleStorageStrategy):
    repr_classname = "AllNilStrategy"
    import_from_mixin(rstrat.SingleValueStrategy)
    def value(self): return self.space.w_nil
AllNilStrategy.instantiate_type = AllNilStrategy

class StrategyFactory(rstrat.StrategyFactory):
    _immutable_fields_ = ["space", "no_specialized_storage"]
    def __init__(self, space):
        from rsqueakvm import objspace
        self.space = space
        self.no_specialized_storage = objspace.ConstantFlag()
        self.singleton_nodes = {}
        rstrat.StrategyFactory.__init__(self, AbstractStrategy)

    # XXX: copied and slightly modified to not set a singleton field on the strategy class
    def _create_strategy_instances(self, root_class, all_strategy_classes):
        for strategy_class in all_strategy_classes:
            if strategy_class._is_strategy:
                self.strategies.append(strategy_class)
            self._patch_strategy_class(strategy_class, root_class)
        self._order_strategies()

    # XXX: copied and slightly modified to include w_class for instantiation from rstrategies
    def switch_strategy(self, w_self, new_strategy_type, new_element=None):
        """
        Switch the strategy of w_self to the new type.
        new_element can be given as as hint, purely for logging purposes.
        It should be the object that was added to w_self, causing the strategy switch.
        """
        old_strategy = self.get_strategy(w_self)
        w_class = old_strategy.getclass()
        if new_strategy_type._is_singleton:
            new_strategy = self.strategy_singleton_instance(new_strategy_type, w_class)
        else:
            size = old_strategy.size(w_self)
            new_strategy = self.instantiate_strategy(new_strategy_type, w_class, w_self, size)
        self.set_strategy(w_self, new_strategy)
        old_strategy._convert_storage_to(w_self, new_strategy)
        new_strategy.strategy_switched(w_self)
        self.log(w_self, new_strategy, old_strategy, new_element)
        return new_strategy

    # XXX: copied and slightly modified to include w_class for instantiation from rstrategies
    def set_initial_strategy(self, w_self, strategy_type, w_class, size, elements=None):
        assert self.get_strategy(w_self) is None, "Strategy should not be initialized yet!"
        if strategy_type._is_singleton:
            strategy = self.strategy_singleton_instance(strategy_type, w_class)
        else:
            strategy = self.instantiate_strategy(strategy_type, w_class, w_self, size)
        self.set_strategy(w_self, strategy)
        strategy._initialize_storage(w_self, size)
        element = None
        if elements:
            strategy.store_all(w_self, elements)
            if len(elements) > 0: element = elements[0]
        strategy.strategy_switched(w_self)
        self.log(w_self, strategy, None, element)
        return strategy

    def strategy_singleton_instance(self, strategy_class, w_class=None):
        s = self.strategy_singleton_instance_from_cache(strategy_class, w_class)
        if s is None:
            s = self.instantiate_strategy(strategy_class, w_class)
            self.singleton_nodes[(strategy_class, w_class)] = s
        return s

    @jit.elidable
    def strategy_singleton_instance_from_cache(self, strategy_class, w_class):
        return self.singleton_nodes.get((strategy_class, w_class), None)

    def instantiate_strategy(self, strategy_type, w_class, w_self=None, initial_size=0):
        return strategy_type(self.space, w_self, initial_size, w_class)

    def strategy_type_for(self, objects, weak=False):
        if weak:
            return WeakListStrategy
        if self.no_specialized_storage.is_set():
            return ListStrategy
        return rstrat.StrategyFactory.strategy_type_for(self, objects)

    def empty_storage_type(self, w_self, size, weak=False):
        if weak:
            return WeakListStrategy
        if self.no_specialized_storage.is_set():
            return ListStrategy
        return AllNilStrategy

    def log(self, w_self, new_strategy, old_strategy=None, new_element=None):
        if not self.logger.active: return
        # Gather information to be logged
        image_loaded = self.space.image_loaded.is_set()
        size = new_strategy.size(w_self)
        new_strategy_str = new_strategy.repr_classname
        old_strategy_str = old_strategy.repr_classname if old_strategy else ""
        classname = w_self.guess_classname() if image_loaded else ""
        element_classname = new_element.guess_classname() if new_element and image_loaded else ""
        if image_loaded:
            cause = "Switched" if old_strategy else "Initialized"
        else:
            cause = "Filledin"
        self.logger.log(new_strategy_str, size, cause, old_strategy_str, classname, element_classname)

# ========== Other storage classes, non-strategies ==========

class ShadowMixin(object):
    """
    Shadows are non-singleton strategies. They maintain a backpointer to their shadowed
    W_PointersObject instance. This is a mixin, because it is used at several places in the
    class tree.
    """
    def w_self(self):
        return self._w_self
    def is_shadow(self):
        return True
    def become(self, w_other):
        w_self = self._w_self
        self.onesided_become(w_other)
        if w_other.has_strategy() and w_other._get_strategy().is_shadow():
            w_other._get_strategy().onesided_become(w_self)
    def onesided_become(self, w_other):
        self._w_self = w_other
    def own_size(self):
        return self.size(self._w_self)
    def own_store(self, i, val):
        self.store(self._w_self, i, val)
    def own_fetch(self, i):
        return self.fetch(self._w_self, i)
    def promote_if_neccessary(self):
        return self

class AbstractGenericShadow(ListStrategy):
    """
    This class behaves just like a generic list storage strategy,
    but allows safe subclassing for more specific, non-singleton strategies.
    """
    _attrs_ = ['_w_self']
    _immutable_fields_ = ['_w_self?']
    import_from_mixin(ShadowMixin)
    def __init__(self, space, w_self, size, w_class):
        ListStrategy.__init__(self, space, w_self, size, w_class)
        assert w_self is None or isinstance(w_self, W_PointersObject)
        self._w_self = w_self
    def _convert_storage_from(self, w_self, previous_strategy):
        # Subclasses need a store() invokation for every field.
        # This 'naive' implementation is available in AbstractStrategy.
        AbstractStrategy._convert_storage_from(self, w_self, previous_strategy)

class AbstractCachingShadow(AbstractGenericShadow):
    """
    Abstract shadow maintaining an empty version object for the
    underlying Smalltalk object. The version object allows jit-related optimizations.
    """
    _immutable_fields_ = ['version?']
    _attrs_ = ['version']
    repr_classname = "AbstractCachingShadow"
    import_from_mixin(VersionMixin)
    version = None

    def __init__(self, space, w_self, size, w_class):
        AbstractGenericShadow.__init__(self, space, w_self, size, w_class)
        self.changed()

class CachedObjectShadow(AbstractCachingShadow):
    """
    A shadow which treats its contents as jit constants as long as the object is not modified.
    """
    repr_classname = "CachedObjectShadow"

    @elidable_for_version(2, promote=True)
    def fetch(self, w_self, n0):
        return AbstractCachingShadow.fetch(self, w_self, n0)

    def store(self, w_self, n0, w_value):
        AbstractCachingShadow.store(self, w_self, n0, w_value)
        self.changed()
CachedObjectShadow.instantiate_type = CachedObjectShadow


class ObserveeShadow(AbstractGenericShadow):
    """
    A generic shadow that notifies a single observer object whenever changes are made.
    """
    _attrs_ = ['observer']
    repr_classname = "ObserveeShadow"
    def __init__(self, space, w_self, size, w_class):
        AbstractGenericShadow.__init__(self, space, w_self, size, w_class)
        self.observer = None

    def store(self, w_self, n0, w_value):
        AbstractGenericShadow.store(self, w_self, n0, w_value)
        if self.observer:
            self.observer.notify()

    def set_observer(self, observer):
        if self.observer is not None and observer is not self.observer:
            raise RuntimeError('Meant to be observed by only one observer, so far')
        self.observer = observer
ObserveeShadow.instantiate_type = ObserveeShadow
