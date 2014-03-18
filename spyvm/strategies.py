from spyvm import model, shadow

from rpython.rlib import rerased
from rpython.rlib import objectmodel, jit, signature
from rpython.rlib.listsort import TimSort
from rpython.rlib.objectmodel import import_from_mixin
from rpython.rlib.debug import make_sure_not_resized

# Disables all optimized strategies, for debugging.
only_list_storage = False

class AbstractStorageStrategy(object):
    _immutable_fields_ = []
    _attrs_ = []
    _settled_ = True
    strategy_tag = 'abstract'
    
    def __init__(self):
        pass
    def needs_objspace(self):
        # Return True, if fetch/store operations use the space parameter.
        # If not, the space-parameter can be passed in as None (probably).
        return False
    
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

# This is a container for an int-value to be used with a rerased-pair
class SizeStorage(object):
    _attrs_ = ['size']
    _settled_ = True
    def __init__(self, size):
        self.size = size

# this is the typical "initial" storage strategy, for when every slot
# in a var-sized object is still nil. No storage is allocated except for
# holding the size of the object.
class AllNilStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    erase, unerase = rerased.new_erasing_pair("all-nil-strategy")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    strategy_tag = 'allnil'
    
    def fetch(self, space, w_obj, n0):
        return model.w_nil
    
    def store(self, space, w_obj, n0, w_val):
        # This is an important moment, where we decide where to go on the first non-nil store.
        if w_val == model.w_nil:
            return
        if not only_list_storage:
            if isinstance(w_val, model.W_SmallInteger):
                return w_obj.store_with_new_strategy(space, DenseSmallIntegerStorageStrategy.singleton, n0, w_val)
        return w_obj.store_with_new_strategy(space, ListStorageStrategy.singleton, n0, w_val)
        
    def size_of(self, w_obj):
        return self.unerase(w_obj.get_storage()).size
    def initial_storage(self, space, size):
        return self.erase(SizeStorage(size))
    def storage_for_list(self, space, collection):
        return self.erase(SizeStorage(len(collection)))
    def copy_storage_from(self, space, w_obj, reuse_storage=False):
        return self.erase(SizeStorage(w_obj.basic_size()))

# This is the regular storage strategy that does not result in any
# optimizations but can handle every case. Applicable for both
# fixed-sized and var-sized objects.
class ListStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    erase, unerase = rerased.new_erasing_pair("list-storage-strategy")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    strategy_tag = 'list'
    
    def get_list(self, w_obj):
        return self.unerase(w_obj.get_storage())
    def fetch(self, space, w_obj, n0):
        return self.get_list(w_obj)[n0]
    def store(self, space, w_obj, n0, w_val):
        # TODO enable generalization by maintaining a counter of elements that are nil.
        self.get_list(w_obj)[n0] = w_val
    def size_of(self, w_obj):
        return len(self.get_list(w_obj))
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
    
class BasicStorageStrategyMixin(object):
    # Concrete class must implement: unerase
    def storage(self, w_obj):
        return self.unerase(w_obj.get_storage())

class DenseStorage(object):
    # Subclass must provide attribute: default_element
    _immutable_fields_ = ['arr']
    _attrs_ = ['arr', '_from', '_to']
    _settled_ = True
    
    def __init__(self, _from, _to, size):
        self._from = _from # first used index ("inclusive")
        self._to = _to # first unused index ("exclusive")
        self.arr = [self.default_element] * size
        make_sure_not_resized(self.arr)

class DenseStorageStrategyMixin(object):
    # Concrete class must implement: storage, erase, do_fetch, do_store, sparse_strategy
    # Concrete class must provide attributes: storage_type (subclass of DenseStorage)
    
    def fetch(self, space, w_obj, n0):
        store = self.storage(w_obj)
        if n0 < store._from or n0 >= store._to:
            return model.w_nil
        return self.do_fetch(space, store.arr, n0)
    def store(self, space, w_obj, n0, w_val):
        store = self.storage(w_obj)
        if not self.can_contain_object(w_val):
            if w_val == model.w_nil:
                if store._to - 1 == n0: # Optimize Collection >> remove:
                    store._to = store._to - 1
                elif n0 < store._from or store._to <= n0:
                    pass # Storing nil to an already-nil position
                elif store._from == n0:
                    store._from = store._from + 1
                else:
                    # Deletion from the middle of the storage. Deoptimize to sparse storage.
                    return w_obj.store_with_new_strategy(space, self.sparse_strategy().singleton, n0, w_val)
                if store._from == store._to:
                    # Deleted last element. Generelize to AllNilStorage.
                    w_obj.switch_strategy(space, AllNilStorageStrategy.singleton)
                return
            else:
                # Storing a non-int - dehomogenize to ListStorage
                return w_obj.store_with_new_strategy(space, ListStorageStrategy.singleton, n0, w_val)
        if n0 == store._to: # Optimize Collection >> add:
            store._to = store._to+1
        elif store._from <= n0 and n0 < store._to:
            pass
        elif n0 == store._from - 1: # It's ok if this wraps around.
            store._from = store._from-1
        else:
            if store._from == store._to:
                # Initial store to non-zero position.
                store._from = n0
                store._to = n0+1
            else:
                # Store to a non-dense position. Deoptimize to sparse storage.
                return w_obj.store_with_new_strategy(space, self.sparse_strategy().singleton, n0, w_val)
        # It is a dense store, so finally store the unwrapped value.
        self.do_store(space, store.arr, n0, w_val)
    def initial_storage(self, space, size):
        return self.erase(self.storage_type(0, 0, size))
    def storage_for_list(self, space, collection):
        _from = 0
        while _from < len(collection) and collection[_from] == model.w_nil:
            _from = _from+1
        _to = _from
        while _to < len(collection) and collection[_to] != model.w_nil:
            _to = _to+1
        store = self.storage_type(_from, _to, len(collection))
        for i in range(_from, _to):
            self.do_store(space, store.arr, i, collection[i])
        return self.erase(store)

class SparseStorage(object):
    _immutable_fields_ = ['arr', 'nil_flags']
    _attrs_ = ['arr', 'nil_flags']
    _settled_ = True
    
    def __init__(self, arr, nil_flags):
        self.arr = arr
        self.nil_flags = nil_flags
        make_sure_not_resized(self.arr)
        make_sure_not_resized(self.nil_flags)
    
class SparseStorageStrategyMixin(object):
    # Concrete class must implement: storage, erase, do_fetch, do_store, dense_strategy
    # Concrete class must provide attributes: storage_type (Subclass of SparseStorage)
    
    def fetch(self, space, w_obj, n0):
        store = self.storage(w_obj)
        if store.nil_flags[n0]:
            return model.w_nil
        return self.do_fetch(space, store.arr, n0)
    def store(self, space, w_obj, n0, w_val):
        store = self.storage(w_obj)
        if not self.can_contain_object(w_val):
            if w_val == model.w_nil:
                # TODO - generelize to AllNilStorage by maintaining a counter of nil-elements
                store.nil_flags[n0] = True
                return
            else:
                # Storing a wrong type - dehomogenize to ListStorage
                return w_obj.store_with_new_strategy(space, ListStorageStrategy.singleton, n0, w_val)
        store.nil_flags[n0] = False
        self.do_store(space, store.arr, n0, w_val)
    def storage_for_size(self, size):
        # TODO -- for inlining strategy, the size must be extended!!
        # size = size * self.slots_per_object()
        return self.storage_type([self.storage_type.default_element] * size, [True] * size)
    def initial_storage(self, space, size):
        return self.erase(self.storage_for_size(size))
    def storage_for_list(self, space, collection):
        length = len(collection)
        store = self.storage_for_size(length)
        for i in range(length):
            if collection[i] != model.w_nil:
                store.nil_flags[i] = False
                self.do_store(space, store.arr, i, collection[i])
        return self.erase(store)
    def copy_storage_from(self, space, w_obj, reuse_storage=False):
        old_strategy = w_obj.strategy
        if isinstance(old_strategy, self.dense_strategy()):
            # Optimized transition from dense to sparse strategy
            store = old_strategy.storage(w_obj)
            return self.erase(self.copy_from_dense_storage(store, reuse_storage))
        else:
            return AbstractStorageStrategy.copy_storage_from(self, space, w_obj, reuse_storage)
    def copy_from_dense_storage(self, store, reuse_storage):
        # TODO possible optimization: compare len(arr) with _to-_from, use smaller iteration size
        nil_flags = [True] * len(store.arr)
        for i in range(store._from, store._to):
            nil_flags[i] = False
        arr = store.arr
        if not reuse_storage:
            arr = [x for x in arr]
        return self.storage_type(arr, nil_flags)

class SmallIntegerStorageStrategyMixin(BasicStorageStrategyMixin):
    def needs_objspace(self):
        return True
    def do_fetch(self, space, arr, n0):
        return space.wrap_int(arr[n0])
    def do_store(self, space, arr, n0, val):
        arr[n0] = space.unwrap_int(val)
    def size_of(self, w_obj):
        return len(self.storage(w_obj).arr)
    def can_contain_object(self, w_val):
        return isinstance(w_val, model.W_SmallInteger)
    
class SparseSmallIntegerStorage(SparseStorage):
    default_element = 0

class SparseSmallIntegerStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    import_from_mixin(SparseStorageStrategyMixin)
    import_from_mixin(SmallIntegerStorageStrategyMixin)
    erase, unerase = rerased.new_erasing_pair("sparse-small-integer-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    strategy_tag = 'sparse-small-int'
    storage_type = SparseSmallIntegerStorage
    def dense_strategy(self):
        return DenseSmallIntegerStorageStrategy
    
class DenseSmallIntegerStorage(DenseStorage):
    default_element = 0

class DenseSmallIntegerStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    import_from_mixin(DenseStorageStrategyMixin)
    import_from_mixin(SmallIntegerStorageStrategyMixin)
    erase, unerase = rerased.new_erasing_pair("dense-small-integer-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    strategy_tag = 'dense-small-int'
    storage_type = DenseSmallIntegerStorage
    def sparse_strategy(self):
        return SparseSmallIntegerStorageStrategy

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
    is_dense = True
    for w_obj in vars:
        if w_obj == model.w_nil:
            if not is_all_nils:
                is_dense = False
        else:
            is_all_nils = False
            if not isinstance(w_obj, model.W_SmallInteger):
                # TODO -- here we can still optimize if there is only
                # one single type in the collection.
                return ListStorageStrategy.singleton
    if is_all_nils:
        return AllNilStorageStrategy.singleton
    if is_dense:
        return DenseSmallIntegerStorageStrategy.singleton
    else:
        return SparseSmallIntegerStorageStrategy.singleton
    