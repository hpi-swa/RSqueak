
import sys
from spyvm import model, constants
from spyvm.util.version import elidable_for_version, VersionMixin
from rpython.rlib import objectmodel, jit
from rpython.rlib.objectmodel import import_from_mixin
import rstrategies as rstrat

class AbstractObjectStorage(object):
    """Subclasses of this handle the information contained in Smalltalk objects.
    The common API allows to store and fetch elements from object slots.
    Every object has some kind of storage representation attached.
    Some subclasses (those with *Shadow in their name) contain additional information,
    required by the VM. These 'shadows' not only manage the memory of their Smalltalk objects,
    but are also the VM-internal representation of these objects.
    """
    _attrs_ = ['_w_self', 'space']
    _immutable_fields_ = ['space']
    provides_getname = False
    repr_classname = "AbstractObjectStorage"
    __metaclass__ = rstrat.StrategyMetaclass
    import_from_mixin(rstrat.AbstractCollection)
    
    def __init__(self, space, w_self, size):
        self.space = space
        assert w_self is None or isinstance(w_self, model.W_PointersObject)
        self._w_self = w_self
    def w_self(self):
        return self._w_self
    def getname(self):
        raise NotImplementedError("Abstract class")
    def __repr__(self):
        if self.provides_getname:
            return "<%s %s>" % (self.repr_classname, self.getname())
        else:
            return "<%s>" % self.repr_classname

# ========== Storage classes implementing storage strategies ==========

class AbstractStrategy(AbstractObjectStorage):
    """
    Strategies handle 'simple' object storage, without additional VM-internal information.
    Depending on the data inside an object, different optimizing strategies are used.
    """
    repr_classname = "AbstractStrategy"
    _attrs_ = []
    import_from_mixin(rstrat.UnsafeIndexingMixin)
    
    def __init__(self, space, w_self, size):
        AbstractObjectStorage.__init__(self, space, w_self, size)
        self.init_strategy(size)
    
    def strategy_factory(self):
        return self.space.strategy_factory
    
    def copy_from_AllNilStrategy(self, all_nil_storage):
        pass # Fields already initialized to nil

    def default_value(self):
        return self.space.w_nil

@rstrat.strategy()
class ListStrategy(AbstractStrategy):
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
        rstrat.StrategyFactory.__init__(self, AbstractObjectStorage)
    
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
    
    def set_initial_strategy(self, w_object, strategy_type, size, elements=None):
        assert w_object.strategy is None, "Shadow should not be initialized yet!"
        strategy = strategy_type(self.space, w_object, size)
        w_object.store_strategy(strategy)
        if elements:
            w_object.store_all(self.space, elements)
        strategy.strategy_switched()
        self.log(strategy)
    
    def instantiate_and_switch(self, old_strategy, size, strategy_class):
        w_self = old_strategy.w_self()
        instance = strategy_class(self.space, w_self, size)
        w_self.store_strategy(instance)
        return instance
    
    def instantiate_empty(self, strategy_type):
        return strategy_type(self.space, None, 0)
    
    def log(self, new_strategy, old_strategy=None, new_element=None):
        if not self.logger.active: return
        # Gather information to be logged
        image_loaded = self.space.image_loaded.is_set()
        size = new_strategy.size()
        new_strategy_str = new_strategy.repr_classname
        old_strategy_str = old_strategy.repr_classname if old_strategy else ""
        classname = new_strategy.w_self().guess_classname() if image_loaded else ""
        element_classname = new_element.guess_classname() if new_element and image_loaded else ""
        if image_loaded:
            cause = "Switched" if old_strategy else "Initialized"
        else:
            cause = "Filledin"
        self.logger.log(new_strategy_str, size, cause, old_strategy_str, classname, element_classname)
    
# ========== Other storage classes, non-strategies ==========

class AbstractRedirectingShadow(AbstractObjectStorage):
    """
    Abstract shadow for handling the object storage in a completely customized way.
    """
    _attrs_ = ['_w_self_size']
    repr_classname = "AbstractRedirectingShadow"

    def __init__(self, space, w_self, size):
        if w_self is not None:
            self._w_self_size = w_self.size()
        else:
            self._w_self_size = size
        AbstractObjectStorage.__init__(self, space, w_self, self._w_self_size)

    def size(self):
        return self._w_self_size

class AbstractCachingShadow(ListStrategy):
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
        ListStrategy.__init__(self, space, w_self, size)
        self.changed()

class CachedObjectShadow(AbstractCachingShadow):
    """
    A shadow which treats its contents as jit constants as the object is not modified.
    """
    repr_classname = "CachedObjectShadow"

    @elidable_for_version
    def fetch(self, n0):
        return AbstractCachingShadow.fetch(self, n0)

    def store(self, n0, w_value):
        AbstractCachingShadow.store(self, n0, w_value)
        self.changed()

class ObserveeShadow(ListStrategy):
    """
    A generic shadow that notifies a single observer object whenever changes are made.
    """
    _attrs_ = ['dependent']
    repr_classname = "ObserveeShadow"
    def __init__(self, space, w_self, size):
        ListStrategy.__init__(self, space, w_self, size)
        self.dependent = None

    def store(self, n0, w_value):
        ListStrategy.store(self, n0, w_value)
        if self.dependent:
            self.dependent.update()

    def notify(self, dependent):
        if self.dependent is not None and dependent is not self.dependent:
            raise RuntimeError('Meant to be observed by only one value, so far')
        self.dependent = dependent
