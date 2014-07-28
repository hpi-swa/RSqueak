
import sys, weakref
from spyvm import model, version, constants
from spyvm.version import elidable_for_version
from rpython.rlib import objectmodel, jit
from rpython.rlib.objectmodel import import_from_mixin

class AbstractShadow(object):
    """A shadow is an optional extra bit of information that
    can be attached at run-time to any Smalltalk object.
    """
    _attrs_ = ['_w_self', 'space']
    _immutable_fields_ = ['space']
    provides_getname = False
    repr_classname = "AbstractShadow"

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

    def fetch(self, n0):
        raise NotImplementedError("Abstract class")
    def store(self, n0, w_value):
        raise NotImplementedError("Abstract class")
    def size(self):
        raise NotImplementedError("Abstract class")

    # This will invoke an appropriate copy_from_* method.
    # Overwriting this allows optimized transitions between certain storage types.
    def copy_into(self, other_shadow):
        other_shadow.copy_from(self)
    
    def attach_shadow(self): pass

    def copy_field_from(self, n0, other_shadow):
        self.store(n0, other_shadow.fetch(n0))

    def copy_from(self, other_shadow):
        assert self.size() == other_shadow.size()
        for i in range(self.size()):
            self.copy_field_from(i, other_shadow)
    
    def copy_from_AllNil(self, all_nil_storage):
        self.copy_from(all_nil_storage)
    def copy_from_SmallIntegerOrNil(self, small_int_storage):
        self.copy_from(small_int_storage)
    def copy_from_FloatOrNil(self, float_storage):
        self.copy_from(float_storage)

# ========== Storage classes implementing storage strategies ==========

class AbstractStorageShadow(AbstractShadow):
    _attrs_ = []
    repr_classname = "AbstractStorageShadow"
    
    def store(self, n0, w_val):
        if self.can_contain(w_val):
            return self.do_store(n0, w_val)
        new_storage = self.generalized_strategy_for(w_val)
        return self._w_self.store_with_new_storage(new_storage, n0, w_val)
    def can_contain(self, w_val):
        return self.static_can_contain(self.space, w_val)
    @staticmethod
    def static_can_contain(space, w_val):
        raise NotImplementedError()
    def do_store(self, n0, w_val):
        raise NotImplementedError()
    def generalized_strategy_for(self, w_val):
        raise NotImplementedError()
    
    def copy_from_AllNil(self, all_nil_storage):
        pass # Already initialized
    def copy_from(self, other_shadow):
        assert self.size() == other_shadow.size()
        for i in range(self.size()):
            w_val = other_shadow.fetch(i)
            if not w_val.is_nil(self.space): # nil fields already initialized
                self.store(i, w_val)

class AllNilStorageShadow(AbstractStorageShadow):
    repr_classname = "AllNilStorageShadow"
    _attrs_ = ['_size']
    _immutable_fields_ = ['_size']
    def __init__(self, space, w_self, size):
        AbstractStorageShadow.__init__(self, space, w_self, size)
        self._size = size
    def fetch(self, n0):
        if n0 >= self._size:
            raise IndexError
        return self.space.w_nil
    def copy_into(self, other_shadow):
        other_shadow.copy_from_AllNil(self)
    def do_store(self, n0, w_value):
        pass
    def size(self):
        return self._size
    def generalized_strategy_for(self, w_val):
        return find_storage_for_objects(self.space, [w_val])
    @staticmethod
    def static_can_contain(space, w_val):
        return isinstance(w_val, model.W_Object) and w_val.is_nil(space)

class AbstractValueOrNilStorageMixin(object):
    # Class must provide: wrap, unwrap, nil_value, is_nil_value, wrapper_class
    _attrs_ = ['storage']
    _immutable_fields_ = ['storage']

    def __init__(self, space, w_self, size):
        AbstractStorageShadow.__init__(self, space, w_self, size)
        self.storage = [self.nil_value] * size

    def size(self):
        return len(self.storage)

    def generalized_strategy_for(self, w_val):
        return ListStorageShadow

    def fetch(self, n0):
        val = self.storage[n0]
        if self.is_nil_value(val):
            return self.space.w_nil
        else:
            return self.wrap(self.space, val)

    def do_store(self, n0, w_val):
        if w_val.is_nil(self.space):
            self.storage[n0] = self.nil_value
        else:
            self.storage[n0] = self.unwrap(self.space, w_val)

# This is to avoid code duplication
@objectmodel.specialize.arg(0)
def _value_or_nil_can_handle(cls, space, w_val):
    return isinstance(w_val, model.W_Object) and w_val.is_nil(space) or \
            (isinstance(w_val, cls.wrapper_class) \
            and not cls.is_nil_value(cls.unwrap(space, w_val)))

class SmallIntegerOrNilStorageShadow(AbstractStorageShadow):
    repr_classname = "SmallIntegerOrNilStorageShadow"
    nil_value = constants.MAXINT
    wrapper_class = model.W_SmallInteger
    import_from_mixin(AbstractValueOrNilStorageMixin)

    @staticmethod
    def static_can_contain(space, w_val):
        return _value_or_nil_can_handle(SmallIntegerOrNilStorageShadow, space, w_val)
    @staticmethod
    def is_nil_value(val):
        return val == SmallIntegerOrNilStorageShadow.nil_value
    @staticmethod
    def wrap(space, val):
        return space.wrap_int(val)
    @staticmethod
    def unwrap(space, w_val):
        return space.unwrap_int(w_val)
    def copy_into(self, other_shadow):
        other_shadow.copy_from_SmallIntegerOrNil(self)

class FloatOrNilStorageShadow(AbstractStorageShadow):
    repr_classname = "FloatOrNilStorageShadow"
    nil_value = sys.float_info.max
    wrapper_class = model.W_Float
    import_from_mixin(AbstractValueOrNilStorageMixin)

    @staticmethod
    def static_can_contain(space, w_val):
        return _value_or_nil_can_handle(FloatOrNilStorageShadow, space, w_val)
    @staticmethod
    def is_nil_value(val):
        return val == FloatOrNilStorageShadow.nil_value
    @staticmethod
    def wrap(space, val):
        return space.wrap_float(val)
    @staticmethod
    def unwrap(space, w_val):
        return space.unwrap_float(w_val)
    def copy_into(self, other_shadow):
        other_shadow.copy_from_FloatOrNil(self)

def empty_storage(space, w_self, size, weak=False):
    if weak:
        return WeakListStorageShadow(space, w_self, size)
    if space.no_specialized_storage.is_set():
        return ListStorageShadow(space, w_self, size)
    return AllNilStorageShadow(space, w_self, size)

@jit.unroll_safe
def find_storage_for_objects(space, vars, weak=False):
    if weak:
        return WeakListStorageShadow
    if space.no_specialized_storage.is_set():
        return ListStorageShadow
    specialized_strategies = 3
    all_nil_can_handle = True
    small_int_can_handle = True
    float_can_handle = True
    for w_obj in vars:
        if all_nil_can_handle and not AllNilStorageShadow.static_can_contain(space, w_obj):
            all_nil_can_handle = False
            specialized_strategies = specialized_strategies - 1
        if small_int_can_handle and not SmallIntegerOrNilStorageShadow.static_can_contain(space, w_obj):
            small_int_can_handle = False
            specialized_strategies = specialized_strategies - 1
        if float_can_handle and not FloatOrNilStorageShadow.static_can_contain(space, w_obj):
            float_can_handle = False
            specialized_strategies = specialized_strategies - 1

        if specialized_strategies <= 0:
            return ListStorageShadow

    if all_nil_can_handle:
        return AllNilStorageShadow
    if small_int_can_handle:
        return SmallIntegerOrNilStorageShadow
    if float_can_handle:
        return FloatOrNilStorageShadow

    # If this happens, please look for a bug in the code above.
    assert False, "No strategy could be found for list..."

class ListStorageMixin(object):
    def __init__(self, space, w_self, size):
        AbstractStorageShadow.__init__(self, space, w_self, size)
        self.initialize_storage(size)
    def size(self):
        return len(self.storage)

class ListStorageShadow(AbstractStorageShadow):
    _attrs_ = ['storage']
    _immutable_fields_ = ['storage']
    repr_classname = "ListStorageShadow"
    import_from_mixin(ListStorageMixin)

    def initialize_storage(self, size):
        self.storage = [self.space.w_nil] * size
    def fetch(self, n0):
        return self.storage[n0]
    def store(self, n0, w_value):
        self.storage[n0] = w_value

class WeakListStorageShadow(AbstractStorageShadow):
    _attrs_ = ['storage']
    _immutable_fields_ = ['storage']
    repr_classname = "WeakListStorageShadow"
    import_from_mixin(ListStorageMixin)

    def initialize_storage(self, size):
        self.storage = [weakref.ref(self.space.w_nil)] * size
    def fetch(self, n0):
        weakobj = self.storage[n0]
        return weakobj() or self.space.w_nil
    def store(self, n0, w_value):
        assert w_value is not None
        self.storage[n0] = weakref.ref(w_value)
        
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
    import_from_mixin(version.VersionMixin)
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
