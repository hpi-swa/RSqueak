
import sys
from spyvm import model, constants
from spyvm.util.version import VersionMixin
from rpython.rlib import objectmodel, jit
from rpython.rlib.objectmodel import import_from_mixin
import rstrategies as rstrat

"""
A note on terminology:
A normal smalltalk objects contains references to other objects. These pointers
are the *storage* of the object. In RSqueak, each W_PointersObject passes 
handling of its storage to a *strategy* object (subclass of AbstractStrategy).
Most objects are handled by a subclass of SingletonStorageStrategy. These classes
implement the actual "storage strategy" concept ("Storage Strategies for Collections
in Dynamically Typed Languages", Bolcz et al., 2013). To support these singleton 
strategies, W_PointersObject have a _storage field for arbitrary usage by their
strategy.
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
    _attrs_ = ['space']
    _immutable_fields_ = ['space']
    provides_getname = False
    repr_classname = "AbstractStrategy"
    __metaclass__ = rstrat.StrategyMetaclass
    import_from_mixin(rstrat.AbstractStrategy)
    
    def __init__(self, space, w_self, size):
        self.space = space
    def getname(self):
        raise NotImplementedError("Abstract class")
    def __repr__(self):
        if self.provides_getname:
            return "<%s %s>" % (self.repr_classname, self.getname())
        else:
            return "<%s>" % self.repr_classname
    def strategy_factory(self):
        return self.space.strategy_factory
    def convert_storage_from_AllNilStrategy(self, w_self, all_nil_storage):
        # Fields are initialized to nil
        self.initialize_storage(w_self, all_nil_storage.size(w_self))
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
        # This is only needed in ShadowProxy, but has to be pulled up here because
        # both AbstractGenericShadow and ContextPartShadow use it.
        raise NotImplementedError("This strategy doesn't handle become.")

# ========== Storage classes implementing storage strategies ==========

class AbstractStrategy(AbstractStrategy):
    """
    Singleton strategies handle 'simple' object storage in normal objects, without
    additional VM-internal information.
    Depending on the data inside an object, different optimizing strategies are used.
    """
    repr_classname = "AbstractStrategy"
    _attrs_ = []
    import_from_mixin(rstrat.UnsafeIndexingMixin)
    
    def default_value(self):
        return self.space.w_nil

@rstrat.strategy()
class ListStrategy(AbstractStrategy):
    _attrs_ = []
    repr_classname = "ListStrategy"
    import_from_mixin(rstrat.GenericStrategy)

class WeakListStrategy(AbstractStrategy):
    repr_classname = "WeakListStrategy"
    import_from_mixin(rstrat.WeakGenericStrategy)

@rstrat.strategy(generalize=[ListStrategy])
class SmallIntegerOrNilStrategy(AbstractStrategy):
    repr_classname = "SmallIntegerOrNilStrategy"
    import_from_mixin(rstrat.TaggingStrategy)
    contained_type = model.W_SmallInteger
    def wrap(self, val): return self.space.wrap_int(val)
    def unwrap(self, w_val): return self.space.unwrap_int(w_val)
    def wrapped_tagged_value(self): return self.space.w_nil
    def unwrapped_tagged_value(self): return constants.MAXINT

@rstrat.strategy(generalize=[ListStrategy])
class FloatOrNilStrategy(AbstractStrategy):
    repr_classname = "FloatOrNilStrategy"
    import_from_mixin(rstrat.TaggingStrategy)
    contained_type = model.W_Float
    tag_float = sys.float_info.max
    def wrap(self, val): return self.space.wrap_float(val)
    def unwrap(self, w_val): return self.space.unwrap_float(w_val)
    def wrapped_tagged_value(self): return self.space.w_nil
    def unwrapped_tagged_value(self): return self.tag_float

@rstrat.strategy(generalize=[
    SmallIntegerOrNilStrategy,
    FloatOrNilStrategy,
    ListStrategy])
class AllNilStrategy(AbstractStrategy):
    repr_classname = "AllNilStrategy"
    import_from_mixin(rstrat.SingleValueStrategy)
    def value(self): return self.space.w_nil

class StrategyFactory(rstrat.StrategyFactory):
    _immutable_fields_ = ["space", "no_specialized_storage"]
    def __init__(self, space):
        from spyvm import objspace
        self.space = space
        self.no_specialized_storage = objspace.ConstantFlag()
        rstrat.StrategyFactory.__init__(self, AbstractStrategy)
    
    def instantiate_strategy(self, strategy_type, w_self=None, initial_size=0):
        return strategy_type(self.space, w_self, initial_size)
    
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

class AbstractGenericShadow(ListStrategy):
    """
    This class behaves just like a generic list storage strategy,
    but allows safe subclassing for more specific, non-singleton strategies.
    """
    _attrs_ = ['_w_self']
    _immutable_fields_ = ['_w_self?']
    import_from_mixin(ShadowMixin)
    def __init__(self, space, w_self, size):
        ListStrategy.__init__(self, space, w_self, size)
        assert w_self is None or isinstance(w_self, model.W_PointersObject)
        self._w_self = w_self
    def convert_storage_from(self, w_self, previous_strategy):
        # Subclasses need a store() invokation for every field.
        # This 'naive' implementation is available in AbstractStrategy.
        AbstractStrategy.convert_storage_from(self, w_self, previous_strategy)

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

    def __init__(self, space, w_self, size):
        AbstractGenericShadow.__init__(self, space, w_self, size)
        self.changed()

class CachedObjectShadow(AbstractCachingShadow):
    """
    A shadow which treats its contents as jit constants as long as the object is not modified.
    """
    repr_classname = "CachedObjectShadow"

    def store(self, w_self, n0, w_value):
        AbstractCachingShadow.store(self, w_self, n0, w_value)
        self.changed()

class ObserveeShadow(AbstractGenericShadow):
    """
    A generic shadow that notifies a single observer object whenever changes are made.
    """
    _attrs_ = ['observer']
    repr_classname = "ObserveeShadow"
    def __init__(self, space, w_self, size):
        AbstractGenericShadow.__init__(self, space, w_self, size)
        self.observer = None

    def store(self, w_self, n0, w_value):
        AbstractGenericShadow.store(self, w_self, n0, w_value)
        if self.observer:
            self.observer.notify()

    def set_observer(self, observer):
        if self.observer is not None and observer is not self.observer:
            raise RuntimeError('Meant to be observed by only one observer, so far')
        self.observer = observer
