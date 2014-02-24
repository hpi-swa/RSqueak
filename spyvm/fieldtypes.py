from spyvm import model, shadow

from rpython.rlib import rerased
from rpython.rlib import objectmodel, jit, signature
from rpython.rlib.listsort import TimSort

class TypeTag():
    pass

LPI = TypeTag()
SInt = TypeTag()
flt = TypeTag()
obj = TypeTag()

# may be used during debugging
# LPI, SInt, flt, obj = 'LPI', 'SInt', 'float', 'object'

class FieldSort(TimSort):
    def lt(self, a, b):
        return a[0] < b[0]

class AbstractStorageStrategy(object):
    _immutable_fields_ = []
    _attrs_ = []
    _settled_ = True

    def __init__(self):
        pass
    def fetch(self, space, w_obj, n0):
        raise NotImplementedError("Abstract base class")
    def store(self, space, w_obj, n0, w_val):
        raise NotImplementedError("Abstract base class")
    
    def fetch_all(self, space, w_obj):
        return [self.fetch(space, w_obj, i) for i in range(self.size_of(w_obj))]
    def store_all(self, space, w_obj, collection):
        # Be tolerant: copy over as many elements as possible, set rest to nil.
        # The size of the object cannot be changed in any case.
        # This should only by used in tests/debugging.
        my_length = self.size_of(w_obj)
        incoming_length = min(my_length, len(collection))
        i = 0
        while i < incoming_length:
            self.store(space, i, w_obj, collection[i])
            i = i+1
        while i < my_length:
            self.store(space, i, w_obj, model.w_nil)
            i = i+1
    
    def size_of(self, w_obj):
        raise NotImplementedError("Abstract base class")
    
    def initial_storage(self, space, size):
        raise NotImplementedError("Abstract base class")
    def storage_for_list(self, space, collection):
        raise NotImplementedError("Abstract base class")
    def copy_storage_from(self, space, w_obj, old_strategy, reuse_storage):
        assert old_strategy != self
        if isinstance(old_strategy, AllNilStorageStrategy):
            return self.initial_storage(space, old_strategy.size_of(w_obj))
        else:
            # Default: reuse storage_for_list() but create intermediate list.
            # Ignore reuse_storage flag - never reuse old storage.
            # Should be overridden & optimized.
            return self.storage_for_list(old_strategy.fetch_all(space, w_obj))

class SingletonMeta(type):
    def __new__(cls, name, bases, dct):
        result = type.__new__(cls, name, bases, dct)
        result.singleton = result()
        return result

# this is the typical "initial" storage strategy, for when every slot
# in a var-sized object is still nil. No storage is allocated except for
# holding the size of the object.
class AllNilStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    erase, unerase = rerased.new_erasing_pair("all-nil-strategy")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    
    def fetch(self, space, w_obj, n0):
        return model.w_nil
    
    def store(self, space, w_obj, n0, w_val):
        # This is an important moment, where we decide where to go on the first non-nil store.
        if w_obj == model.w_nil:
            return
        if isinstance(w_obj, model.W_SmallInteger):
            return w_obj.store_with_new_strategy(space, DenseSmallIntegerStorageStrategy.singleton, n0, w_val)
        return w_obj.store_with_new_strategy(space, ListStorageStrategy.singleton, n0, w_val)
        
    def fetch_all(self, space, w_obj):
        return [model.w_nil] * self.size_of(w_obj)
    def size_of(self, w_obj):
        return self.unerase(w_obj.storage)
    def initial_storage(self, space, size):
        return self.erase(size)
    def storage_for_list(self, space, collection):
        return self.erase(len(collection))
    def copy_storage_from(self, space, w_obj, old_strategy, reuse_storage):
        return self.erase(old_strategy.size_of(w_obj))

# This is the regular storage strategy that does not result in any
# optimizations but can handle every case. Applicable for both
# fixed-sized and var-sized objects.
class ListStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    erase, unerase = rerased.new_erasing_pair("list-storage-strategy")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    
    def fetch(self, space, w_obj, n0):
        return self.unerase(w_obj.storage)[n0]
    def store(self, space, w_obj, n0, w_val):
        self.unerase(w_obj.storage)[n0] = w_val
    def fetch_all(self, space, w_obj):
        return self.unerase(w_obj.storage)
    def size_of(self, w_obj):
        return len(self.unerase(w_obj.storage))
    def initial_storage(self, space, size):
        return self.erase([model.w_nil] * size)
    def storage_for_list(self, space, collection):
        return self.erase([x for x in collection])
    def copy_storage_from(self, space, w_obj, old_strategy, reuse_storage):
        length = old_strategy.size_of(w_obj)
        return self.erase([old_strategy.fetch(space, w_obj, i) for i in range(length)])

class DenseSmallIntegerStorage(object):
    _immutable_fields_ = ['arr']
    _attrs_ = ['arr', '_from', '_to']
    _settled_ = True
    
    def __init__(self, _from, _to, size):
        self._from = _from # first used index ("inclusive")
        self._to = _to # first unused index ("exclusive")
        self.arr = [0] * size

class DenseSmallIntegerStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    erase, unerase = rerased.new_erasing_pair("dense-small-integer-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    
    def fetch(self, space, w_obj, n0):
        store = self.unerase(w_obj.storage)
        if n0 < store._from or n0 >= store._to:
            return model.w_nil
        return space.wrap_int(store.arr[n0])
    def store(self, space, w_obj, n0, w_val):
        store = self.unerase(w_obj.storage)
        if not isinstance(w_val, model.W_SmallInteger):
            if w_val == model.w_nil:
                if store._to - 1 == n0:
                    store._to = store._to - 1
                elif store._from <= n0 and n0 < store._to:
                    pass
                elif store._from == n0:
                    store._from = store._from + 1
                else:
                    # Deletion from a non-dense position. Deoptimize to SparseSmallIntegerStorage.
                    return w_obj.store_with_new_strategy(space, SparseSmallIntegerStorageStrategy.singleton, n0, w_val)
                if store._from == store._to:
                    # Deleted last element. Generelize to AllNilStorage.
                    w_obj.switch_strategy(space, AllNilStorageStrategy.singleton)
                return
            else:
                # Storing a non-int - dehomogenize to ListStorage
                return w_obj.store_with_new_strategy(space, ListStorageStrategy.singleton, n0, w_val)
        if n0 == store._from - 1: # It's ok if this wraps around.
            store._from = store._from-1
        elif n0 >= store._from and n0 < store._to:
            pass
        elif n0 == store._to:
            store._to = store._to+1
        else:
            if store._from == store._to:
                # Initial store to non-zero position.
                store._from = n0
                store._to = n0+1
            else:
                # Store to a non-dense position. Deoptimize to SparseSmallIntegerStorage.
                return w_obj.store_with_new_strategy(space, n0, SparseSmallIntegerStorageStrategy.singleton, w_val)
        # It is a dense store, so finally store the unwrapped int.
        store.arr[n0] = space.unwrap_int(w_val)
    def size_of(self, w_obj):
        return len(self.unerase(w_obj.storage).arr)
    def initial_storage(self, space, size):
        return self.erase(DenseSmallIntegerStorage(0, 0, size))
    def storage_for_list(self, collection):
        _from = 0
        while _from < len(collection) and collection[_from] == model.w_nil:
            _from = _from+1
        _to = _from
        while _to < len(collection) and collection[_to] != model.w_nil:
            _to = _to+1
        store = DenseSmallIntegerStorage(_from, _to, len(collection))
        for i in range(_from, _to):
            store.arr[i] = space.unwrap_int(collection[i])
        return self.erase(store)

class SparseSmallIntegerStorage(object):
    _immutable_fields_ = ['arr', 'nil_flags']
    _attrs_ = ['arr', 'nil_flags']
    _settled_ = True
    
    def __init__(self, arr, nil_flags):
        self.arr = arr
        self.nil_flags = nil_flags
    
    @classmethod
    def for_size(cls, size):
        return cls([0] * size, [True] * size)

class SparseSmallIntegerStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    erase, unerase = rerased.new_erasing_pair("sparse-small-integer-strategry")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    
    def fetch(self, space, w_obj, n0):
        store = self.unerase(w_obj.storage)
        if store.nil_flags[n0]:
            return model.w_nil
        return store.arr[n0]
    def store(self, space, w_obj, n0, w_val):
        store = self.unerase(w_obj.storage)
        if not isinstance(w_val, model.W_SmallInteger):
            if w_val == model.w_nil:
                # TODO - generelize to AllNilStorage by maintaining a counter of nil-elements
                store.nil_flags[n0] = True
                return
            else:
                # Storing a non-int - dehomogenize to ListStorage
                return w_obj.store_with_new_strategy(space, ListStorageStrategy.singleton, n0, w_val)
        store.nil_flags[n0] = False
        store.arr[n0] = space.unwrap_int(w_val)
    def size_of(self, w_obj):
        return len(self.unerase(w_obj.storage).arr)
    def initial_storage(self, space, size):
        return self.erase(SparseSmallIntegerStorage(size))
    def storage_for_list(self, space, collection):
        length = len(collection)
        store = SparseSmallIntegerStorage(length)
        for i in range(length):
            if collection[i] == model.w_nil:
                store.nil_flags[i] = True
            else:
                store.nil_flags[i] = False
                store.arr[i] = space.unwrap_int(collection[i])
        return self.erase(store)
    def copy_storage_from(self, space, w_obj, old_strategy, reuse):
        if isinstance(old_strategy, DenseSmallIntegerStorageStrategy):
            # Optimize transition from Dense to Sparse small-integer-storage
            store = old_strategy.unerase(w_obj.storage)
            nil_flags = [ (i < store._from and i >= store._to) for i in range(store.arr) ]
            arr = store.arr
            if not reuse:
                arr = [x for x in arr]
            return self.erase(SparseSmallIntegerStorage(arr, nil_flags))
        else:
            return AbstractStorageStrategy.copy_storage_from(self, space, w_obj, old_strategy, reuse)

class FixedSizeFieldTypes(AbstractStorageStrategy):
    _immutable_fields_ = ['types[*]']
    _attrs_ = ['types', 'parent', 'siblings', 'diff']
    _settled_ = True
    
    erase, unerase = rerased.new_erasing_pair("fixed-size-field-types")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    
    def __init__(self, types, parent=None, change=(-1, obj)):
        self.types = types
        self.parent = parent
        if parent is not None:
            assert change != (-1, obj)
        self.diff = change
        self.siblings = {}
    
    def initial_storage(self, space, size):
        return self.erase([model.w_nil] * size)
    
    def storage_for_list(self, space, collection):
        return self.erase([x for x in collection])
    
    def size_of(self, w_obj):
        return len(self.unerase(w_obj.storage))
    
    def fetch(self, space, w_object, n0):
        w_result = self.unerase(w_object.storage)[n0]
        assert w_result is not None
        types = self.types
        # TODO - try 'assert isinstance' instead.
        if types[n0] is SInt:
            jit.record_known_class(w_result, model.W_SmallInteger)
        elif types[n0] is LPI:
            jit.record_known_class(w_result, model.W_LargePositiveInteger1Word)
        elif types[n0] is flt:
            jit.record_known_class(w_result, model.W_Float)
        return w_result

    def store(self, space, w_object, n0, w_value):
        types = self.types
        changed_type = w_value.fieldtype()
        if types[n0] is not changed_type:
            w_object.strategy = self.sibling(n0, changed_type)
        self.unerase(w_object.storage)[n0] = w_value

    @jit.elidable
    def sibling(self, n0, changed_type):
        assert self.types[n0] is not changed_type
        change = (n0, changed_type)
        parent = self.parent
        siblings = self.siblings
        if change in siblings:
            return siblings[change]
        elif parent is None:
            return self.descent([change])
        else:
            if n0 == self.diff[0]:
                diff = [change]
            else:
                diff = [change, self.diff]

            new_fieldtype = parent.ascent(diff)

            if not objectmodel.we_are_translated():
                new_types = list(self.types)
                new_types[n0] = changed_type
                assert new_fieldtype.types == new_types
            siblings[change] = new_fieldtype
            return new_fieldtype

    def ascent(self, changes):
        parent = self.parent
        if parent is None:
            FieldSort(changes).sort()
            return self.descent(changes)
        else:
            change = self.diff
            if changes[0][0] != change[0]:
                changes.append(change)
            return parent.ascent(changes)

    def descent(self, changes):
        if changes == []:
            return self

        change = changes[0]
        if change[1] is obj:
            return self.descent(changes[1:])
        siblings = self.siblings
        if change in siblings:
            return siblings[change].descent(changes[1:])
        else:
            new_types = list(self.types)
            assert new_types[change[0]] == obj
            new_types[change[0]] = change[1]
            new_fieldtype = FixedSizeFieldTypes(new_types, self, change)
            siblings[change] = new_fieldtype
            return new_fieldtype.descent(changes[1:])

    @staticmethod
    @jit.elidable
    def of_size(n):
        if n not in maps:
            maps[n] = FixedSizeFieldTypes([obj] * n)
        return maps[n]

maps = {}

def strategy_of_size(s_containing_class, size):
    if s_containing_class is None:
        # This is a weird and rare special case for w_nil
        return ListStorageStrategy.singleton
    if s_containing_class.isvariable():
        return AllNilStorageStrategy.singleton
    else:
        # TODO -- use strategy-concept for fixed-size classes also.
        # TODO -- monitor classes and create heuristic-based default strategies 
        # that should be more suited. Example: 2 small-integers for Point.
        return FixedSizeFieldTypes.of_size(size)

def strategy_for_list(s_containing_class, vars):
    if s_containing_class is None:
        # This is a weird and rare special case for w_nil
        return ListStorageStrategy.singleton
    if s_containing_class.isvariable():
        return AllNilStorageStrategy.singleton
    else:
        size = len(vars)
        typer = FixedSizeFieldTypes.of_size(size)
        for i, w_val in enumerate(vars):
            changed_type = w_val.fieldtype()
            if changed_type is not obj:
                typer = typer.sibling(i, changed_type)
        return typer
