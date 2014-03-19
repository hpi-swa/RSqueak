from spyvm import model, shadow

from rpython.rlib import rerased
from rpython.rlib import objectmodel, jit, signature
from rpython.rlib.objectmodel import import_from_mixin

class AbstractStorageStrategy(object):
    _immutable_fields_ = []
    _attrs_ = []
    _settled_ = True

    def fetch(self, space, w_obj, n0):
        raise NotImplementedError("Abstract base class")
    def store(self, space, w_obj, n0, w_val):
        raise NotImplementedError("Abstract base class")
    def size_of(self, w_obj):
        raise NotImplementedError("Abstract base class")
    
    def initial_storage(self, space, size):
        raise NotImplementedError("Abstract base class")
    def storage_for_list(self, space, collection):
        raise NotImplementedError("Abstract base class")
    def copy_storage_from(self, space, w_obj, reuse_storage=False):
        old_strategy = w_obj.strategy
        if old_strategy == self and reuse_storage:
            return w_obj.get_storage()
        if isinstance(old_strategy, AllNilStorageStrategy):
            return self.initial_storage(space, old_strategy.size_of(w_obj))
        else:
            # This can be overridden and optimized (reuse_storage flag, less temporary storage)
            return self.storage_for_list(space, w_obj.fetch_all(space))

class SingletonMeta(type):
    def __new__(cls, name, bases, dct):
        result = type.__new__(cls, name, bases, dct)
        result.singleton = result()
        return result

class BasicStorageStrategyMixin(object):
    # Concrete class must implement: unerase
    def storage(self, w_obj):
        return self.unerase(w_obj.get_storage())

# This is the regular storage strategy that does not result in any
# optimizations but can handle every case. Applicable for both
# fixed-sized and var-sized objects.
class ListStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    erase, unerase = rerased.new_static_erasing_pair("list-storage-strategy")
    import_from_mixin(BasicStorageStrategyMixin)
    
    def fetch(self, space, w_obj, n0):
        return self.storage(w_obj)[n0]
    def store(self, space, w_obj, n0, w_val):
        self.storage(w_obj)[n0] = w_val
    def size_of(self, w_obj):
        return len(self.storage(w_obj))
    def initial_storage(self, space, size):
        return self.erase([model.w_nil] * size)
    def storage_for_list(self, space, collection):
        return self.erase([x for x in collection])

class TaggingSmallIntegerStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    erase, unerase = rerased.new_static_erasing_pair("tagging-small-integer-strategry")
    import_from_mixin(BasicStorageStrategyMixin)
    
    @staticmethod
    def wrap(val):
        return val << 1
    @staticmethod
    def unwrap(val):
        return val >> 1
    @staticmethod
    def is_nil(val):
        return (val & 1) == 1
    @staticmethod
    def can_contain(w_val):
        return isinstance(w_val, model.W_SmallInteger)
    # TODO - use just a single value to represent nil (max_int-1)
    # Then, turn wrap/unwrap into noops
    # also store W_LargePositiveInteger1Word?
    nil_value = 1
    
    def needs_objspace(self):
        return True
        
    def fetch(self, space, w_obj, n0):
        val = self.storage(w_obj)[n0]
        if (self.is_nil(val)):
            return space.w_nil
        else:
            return space.wrap_int(self.unwrap(val))
        
    def store(self, space, w_obj, n0, w_val):
        store = self.storage(w_obj)
        if self.can_contain(w_val):
            store[n0] = self.wrap(space.unwrap_int(w_val))
        else:
            if w_val == model.w_nil:
                # TODO - generelize to AllNilStorage by maintaining a counter of nil-elements
                store[n0] = self.nil_value
            else:
                # Storing a wrong type - dehomogenize to ListStorage
                return w_obj.store_with_new_strategy(space, ListStorageStrategy.singleton, n0, w_val)
        
    def size_of(self, w_obj):
        return len(self.storage(w_obj))
    
    def initial_storage(self, space, size):
        return self.erase([self.nil_value] * size)
    
    def storage_for_list(self, space, collection):
        length = len(collection)
        store = [self.nil_value] * length
        for i in range(length):
            if collection[i] != model.w_nil:
                store[i] = self.wrap(space.unwrap_int(collection[i]))
        return self.erase(store)

def strategy_of_size(s_containing_class, size):
    if s_containing_class is None:
        # This is a weird and rare special case for w_nil
        return ListStorageStrategy.singleton
    if not s_containing_class.isvariable():
        return ListStorageStrategy.singleton
    
    # A newly allocated object contains only nils.
    # return AllNilStorageStrategy.singleton
    return ListStorageStrategy.singleton

def strategy_for_list(s_containing_class, vars):
    if s_containing_class is None:
            # This is a weird and rare special case for w_nil
            return ListStorageStrategy.singleton
    try:
        is_variable = s_containing_class.isvariable()
    except AttributeError:
        # TODO - This happens during bootstrapping phase, when filling in generic objects.
        # Ths class object shadows are not yet synchronized.
        return ListStorageStrategy.singleton
    
    if not is_variable:
        return ListStorageStrategy.singleton
    
    is_all_nils = True
    for w_obj in vars:
        if w_obj != model.w_nil:
            is_all_nils = False
            if not TaggingSmallIntegerStorageStrategy.can_contain(w_obj):
                # TODO -- here we can still optimize if there is only
                # one single type in the collection.
                return ListStorageStrategy.singleton
    if is_all_nils:
        return ListStorageStrategy.singleton
        # return AllNilStorageStrategy.singleton
    else:
        return TaggingSmallIntegerStorageStrategy.singleton