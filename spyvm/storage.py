
import sys
from spyvm import model, constants
from spyvm.util.version import elidable_for_version, VersionMixin
from rpython.rlib import objectmodel, jit
from rpython.rlib.objectmodel import import_from_mixin
import rstrategies as rstrat

class AbstractShadow(object):
    """A shadow is an optional extra bit of information that
    can be attached at run-time to any Smalltalk object.
    """
    _attrs_ = ['_w_self', 'space']
    _immutable_fields_ = ['space']
    provides_getname = False
    repr_classname = "AbstractShadow"
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

    def s_become(self, w_self, w_other):
        if w_other.shadow:
            w_other.shadow._s_become(w_other, self, w_self)
        else:
            self._w_self = w_self

    def _s_become(self, w_self, s_other, w_other):
        self._w_self, s_other._w_self = w_self, w_other

# ========== Storage classes implementing storage strategies ==========

class AbstractStorageShadow(AbstractShadow):
    repr_classname = "AbstractStorageShadow"
    _attrs_ = []
    import_from_mixin(rstrat.UnsafeIndexingMixin)

    def __init__(self, space, w_self, size):
        AbstractShadow.__init__(self, space, w_self, size)
        self.init_strategy(size)

    def strategy_factory(self):
        return self.space.strategy_factory

    def copy_from_AllNilStorageShadow(self, all_nil_storage):
        pass # Fields already initialized to nil

    def default_value(self):
        return self.space.w_nil

@rstrat.strategy()
class ListStorageShadow(AbstractStorageShadow):
    repr_classname = "ListStorageShadow"
    import_from_mixin(rstrat.GenericStrategy)

class WeakListStorageShadow(AbstractStorageShadow):
    repr_classname = "WeakListStorageShadow"
    import_from_mixin(rstrat.WeakGenericStrategy)

@rstrat.strategy(generalize=[ListStorageShadow])
class SmallIntegerOrNilStorageShadow(AbstractStorageShadow):
    repr_classname = "SmallIntegerOrNilStorageShadow"
    import_from_mixin(rstrat.TaggingStrategy)
    contained_type = model.W_SmallInteger
    def wrap(self, val): return self.space.wrap_int(val)
    def unwrap(self, w_val): return self.space.unwrap_int(w_val)
    def wrapped_tagged_value(self): return self.space.w_nil
    def unwrapped_tagged_value(self): return constants.MAXINT

@rstrat.strategy(generalize=[ListStorageShadow])
class FloatOrNilStorageShadow(AbstractStorageShadow):
    repr_classname = "FloatOrNilStorageShadow"
    import_from_mixin(rstrat.TaggingStrategy)
    contained_type = model.W_Float
    tag_float = sys.float_info.max
    def wrap(self, val): return self.space.wrap_float(val)
    def unwrap(self, w_val): return self.space.unwrap_float(w_val)
    def wrapped_tagged_value(self): return self.space.w_nil
    def unwrapped_tagged_value(self): return self.tag_float

@rstrat.strategy(generalize=[
    SmallIntegerOrNilStorageShadow,
    FloatOrNilStorageShadow,
    ListStorageShadow])
class AllNilStorageShadow(AbstractStorageShadow):
    repr_classname = "AllNilStorageShadow"
    import_from_mixin(rstrat.SingleValueStrategy)
    def value(self): return self.space.w_nil

class StrategyFactory(rstrat.StrategyFactory):
    _immutable_fields_ = ["space", "no_specialized_storage"]
    def __init__(self, space):
        from spyvm import objspace
        self.space = space
        self.no_specialized_storage = objspace.ConstantFlag()
        rstrat.StrategyFactory.__init__(self, AbstractShadow)

    def strategy_type_for(self, objects, weak=False):
        if weak:
            return WeakListStorageShadow
        if self.no_specialized_storage.is_set():
            return ListStorageShadow
        return rstrat.StrategyFactory.strategy_type_for(self, objects)

    def empty_storage_type(self, w_self, size, weak=False):
        if weak:
            return WeakListStorageShadow
        if self.no_specialized_storage.is_set():
            return ListStorageShadow
        return AllNilStorageShadow

    def set_initial_strategy(self, w_object, strategy_type, size, elements=None):
        assert w_object.shadow is None, "Shadow should not be initialized yet!"
        strategy = strategy_type(self.space, w_object, size)
        w_object.store_shadow(strategy)
        if elements:
            w_object.store_all(self.space, elements)
        strategy.strategy_switched()
        self.log(strategy)

    def instantiate_and_switch(self, old_strategy, size, strategy_class):
        w_self = old_strategy.w_self()

        # XXX: Special handling for contexts
        from spyvm.storage_contexts import BlockContextMarkerClass, MethodContextMarkerClass, ContextPartShadow
        if strategy_class is BlockContextMarkerClass:
            instance = ContextPartShadow(self.space, w_self, size, is_block_context=True)
        elif strategy_class is MethodContextMarkerClass:
            instance = ContextPartShadow(self.space, w_self, size, is_block_context=False)
        else:
            instance = strategy_class(self.space, w_self, size)

        w_self.store_shadow(instance)
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

class AbstractRedirectingShadow(AbstractShadow):
    _attrs_ = ['_w_self_size']
    repr_classname = "AbstractRedirectingShadow"

    def __init__(self, space, w_self, size):
        if w_self is not None:
            self._w_self_size = w_self.size()
        else:
            self._w_self_size = size
        AbstractShadow.__init__(self, space, w_self, self._w_self_size)

    def size(self):
        return self._w_self_size

class AbstractCachingShadow(ListStorageShadow):
    _immutable_fields_ = ['version?']
    _attrs_ = ['version']
    repr_classname = "AbstractCachingShadow"
    import_from_mixin(VersionMixin)
    version = None

    def __init__(self, space, w_self, size):
        ListStorageShadow.__init__(self, space, w_self, size)
        self.changed()

class CachedObjectShadow(AbstractCachingShadow):
    repr_classname = "CachedObjectShadow"

    @elidable_for_version
    def fetch(self, n0):
        return AbstractCachingShadow.fetch(self, n0)

    def store(self, n0, w_value):
        AbstractCachingShadow.store(self, n0, w_value)
        self.changed()

class ObserveeShadow(ListStorageShadow):
    _attrs_ = ['dependent']
    repr_classname = "ObserveeShadow"
    def __init__(self, space, w_self, size):
        ListStorageShadow.__init__(self, space, w_self, size)
        self.dependent = None

    def store(self, n0, w_value):
        ListStorageShadow.store(self, n0, w_value)
        if self.dependent:
            self.dependent.update()

    def notify(self, dependent):
        if self.dependent is not None and dependent is not self.dependent:
            raise RuntimeError('Meant to be observed by only one value, so far')
        self.dependent = dependent
