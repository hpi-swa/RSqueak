from spyvm import model, shadow

from rpython.rlib import rerased, objectmodel, jit, signature
from rpython.rlib.objectmodel import import_from_mixin
from rpython.rlib.debug import make_sure_not_resized

# Disables all optimized strategies, for debugging.
only_list_storage = False

class AbstractStorageStrategy(object):
    _immutable_fields_ = []
    _attrs_ = []
    _settled_ = True
    strategy_tag = 'abstract'
    uses_int_storage = False
    
    def __init__(self):
        pass
    def needs_objspace(self):
        # Return True, if fetch/store operations use the space parameter.
        # If not, the space-parameter can be passed in as None (probably).
        return False
    
    def set_initial_storage(self, space, w_obj, size):
        if self.uses_int_storage:
            w_obj.int_storage = self.initial_int_storage(space, size)
        else:
            w_obj.list_storage = self.initial_storage(space, size)
    
    def set_storage_for_list(self, space, w_obj, collection):
        if self.uses_int_storage:
            w_obj.int_storage = self.int_storage_for_list(space, collection)
        else:
            w_obj.list_storage = self.storage_for_list(space, collection)
    
    def set_storage_copied_from(self, space, w_obj, w_source_obj, reuse_storage=False):
        if self.uses_int_storage:
            w_obj.int_storage = self.copy_int_storage_from(space, w_source_obj, reuse_storage)
        else:
            w_obj.list_storage = self.copy_storage_from(space, w_source_obj, reuse_storage)
    
    def fetch(self, space, w_obj, n0):
        raise NotImplementedError("Abstract base class")
    def store(self, space, w_obj, n0, w_val):
        raise NotImplementedError("Abstract base class")
    def size_of(self, w_obj):
        raise NotImplementedError("Abstract base class")
    
    def initial_storage(self, space, size):
        raise NotImplementedError("Abstract base class")
    def initial_int_storage(self, space, size):
        raise NotImplementedError("Abstract base class")
    def storage_for_list(self, space, collection):
        raise NotImplementedError("Abstract base class")
    def int_storage_for_list(self, space, collection):
        raise NotImplementedError("Abstract base class")
    def copy_int_storage_from(self, space, w_obj, reuse_storage):
        old_strategy = w_obj.strategy
        if old_strategy == self and reuse_storage:
            return w_obj.int_storage
        else:
            # This can be overridden and optimized (reuse_storage flag, less temporary storage)
            return self.int_storage_for_list(space, w_obj.fetch_all(space))
    def copy_storage_from(self, space, w_obj, reuse_storage):
        old_strategy = w_obj.strategy
        if old_strategy == self and reuse_storage:
            return w_obj.list_storage
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
    def size_of(self, w_obj):
        if self.uses_int_storage:
            return len(self.int_storage(w_obj))
        else:
            return len(self.storage(w_obj))
    def int_storage(self, w_obj):
        return w_obj.int_storage
    def storage(self, w_obj):
        return w_obj.list_storage
    def erase(self, a): return a
    def unerase(self, a): return a

# this is the typical "initial" storage strategy, for when every slot
# in a var-sized object is still nil. No storage is allocated except for
# holding the size of the object.
class AllNilStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    # erase, unerase = rerased.new_static_erasing_pair("all-nil-strategy")
    import_from_mixin(BasicStorageStrategyMixin)
    strategy_tag = 'allnil'
    
    def fetch(self, space, w_obj, n0):
        return model.w_nil
    
    def store(self, space, w_obj, n0, w_val):
        # This is an important moment, where we decide where to go on the first non-nil store.
        if w_val == model.w_nil:
            return
        if not only_list_storage:
            if TaggingSmallIntegerStorageStrategy.can_contain(w_val):
                return w_obj.store_with_new_strategy(space, TaggingSmallIntegerStorageStrategy.singleton, n0, w_val)
        return w_obj.store_with_new_strategy(space, ListStorageStrategy.singleton, n0, w_val)
        
    def initial_storage(self, space, size):
        return []
    def storage_for_list(self, space, collection):
        return []
    def copy_storage_from(self, space, w_obj, reuse_storage=False):
        return []

# This is the regular storage strategy that does not result in any
# optimizations but can handle every case. Applicable for both
# fixed-sized and var-sized objects.
class ListStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    # erase, unerase = rerased.new_static_erasing_pair("list-storage-strategy")
    import_from_mixin(BasicStorageStrategyMixin)
    strategy_tag = 'list'
    
    def fetch(self, space, w_obj, n0):
        return self.storage(w_obj)[n0]
    def store(self, space, w_obj, n0, w_val):
        # TODO enable generalization by maintaining a counter of elements that are nil.
        self.storage(w_obj)[n0] = w_val
    def erased_list(self, list):
        make_sure_not_resized(list)
        return self.erase(list)
    def initial_storage(self, space, size):
        return self.erased_list([model.w_nil] * size)
    def storage_for_list(self, space, collection):
        return self.erased_list([x for x in collection])
    def copy_storage_from(self, space, w_obj, reuse_storage=False):
        length = w_obj.basic_size()
        return self.erased_list([w_obj.strategy.fetch(space, w_obj, i) for i in range(length)])

class TaggingSmallIntegerStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    strategy_tag = 'tagging'
    # erase, unerase = rerased.new_static_erasing_pair("tagging-small-integer-strategry")
    import_from_mixin(BasicStorageStrategyMixin)
    uses_int_storage = True
    
    @staticmethod
    def wrap(val):
        return val << 1
    @staticmethod
    def unwrap(val):
        return val >> 1
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
        val = self.int_storage(w_obj)[n0]
        if val == self.nil_value:
            return space.w_nil
        else:
            return space.wrap_int(self.unwrap(val))
        
    def store(self, space, w_obj, n0, w_val):
        store = self.int_storage(w_obj)
        if self.can_contain(w_val):
            store[n0] = self.wrap(space.unwrap_int(w_val))
        else:
            if w_val == space.w_nil:
                # TODO - generelize to AllNilStorage by maintaining a counter of nil-elements
                store[n0] = self.nil_value
            else:
                # Storing a wrong type - dehomogenize to ListStorage
                return w_obj.store_with_new_strategy(space, ListStorageStrategy.singleton, n0, w_val)
        
    def initial_int_storage(self, space, size):
        return self.erase([self.nil_value] * size)
    
    def int_storage_for_list(self, space, collection):
        length = len(collection)
        store = [self.nil_value] * length
        for i in range(length):
            if collection[i] != space.w_nil:
                store[i] = self.wrap(space.unwrap_int(collection[i]))
        return self.erase(store)

def strategy_of_size(s_containing_class, size):
    if s_containing_class is None:
        # This is a weird and rare special case for w_nil
        return ListStorageStrategy.singleton
    if not s_containing_class.isvariable() or only_list_storage:
        return ListStorageStrategy.singleton
    
    # A newly allocated object contains only nils.
    return AllNilStorageStrategy.singleton

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
    
    if not is_variable or only_list_storage:
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
        return AllNilStorageStrategy.singleton
    else:
        return TaggingSmallIntegerStorageStrategy.singleton
    