
import sys, math
from spyvm import model, shadow, constants
from rpython.rlib import longlong2float, rarithmetic
from rpython.rlib.rstruct.runpack import runpack
from rpython.rtyper.lltypesystem import rffi, lltype
from rpython.rlib.objectmodel import import_from_mixin
from rpython.rlib.rfloat import string_to_float

class AbstractStorageStrategy(object):
    _immutable_fields_ = []
    _attrs_ = []
    _settled_ = True
    strategy_tag = 'abstract'
    needs_objspace = False
    
    def set_initial_storage(self, space, w_obj, size):
        raise NotImplementedError("Abstract base class")
    def set_storage_for_list(self, space, w_obj, collection):
        raise NotImplementedError("Abstract base class")
    def set_storage_copied_from(self, space, w_obj, w_source_obj, reuse_storage=False):
        raise NotImplementedError("Abstract base class")
    
    def store(self, space, w_obj, n0, w_val):
        if self.can_contain(space, w_val):
            return self.do_store(space, w_obj, n0, w_val)
        new_strategy = self.generelized_strategy_for(space, w_val)
        return w_obj.store_with_new_strategy(space, new_strategy, n0, w_val)
    
    def generelized_strategy_for(self, space, w_val):
        raise NotImplementedError("Abstract base class")
    def can_contain(self, space, w_val):
        raise NotImplementedError("Abstract base class")
    def fetch(self, space, w_obj, n0):
        raise NotImplementedError("Abstract base class")
    def do_store(self, space, w_obj, n0, w_val):
        raise NotImplementedError("Abstract base class")

class AbstractListStorageStrategy(AbstractStorageStrategy):
    strategy_tag = 'abstract-list'
    
    def storage(self, w_obj):
        return w_obj.list_storage
    def set_initial_storage(self, space, w_obj, size):
        w_obj.list_storage = self.initial_storage(space, size)
    def set_storage_for_list(self, space, w_obj, collection):
        w_obj.list_storage = self.storage_for_list(space, collection)
    def set_storage_copied_from(self, space, w_obj, w_source_obj, reuse_storage=False):
        w_obj.list_storage = self.copy_storage_from(space, w_source_obj, reuse_storage)
    
    def initial_storage(self, space, size):
        raise NotImplementedError("Abstract base class")
    def storage_for_list(self, space, collection):
        raise NotImplementedError("Abstract base class")
    def copy_storage_from(self, space, w_obj, reuse_storage):
        old_strategy = w_obj.strategy
        if old_strategy == self and reuse_storage:
            return self.storage(w_obj)
        else:
            # This can be overridden and optimized (reuse_storage flag, less temporary storage)
            return self.storage_for_list(space, w_obj.fetch_all(space))
        
class AbstractIntStorageStrategy(AbstractStorageStrategy):
    strategy_tag = 'abstract-int'
    
    def storage(self, w_obj):
        return w_obj.int_storage
    def set_initial_storage(self, space, w_obj, size):
        w_obj.int_storage = self.initial_storage(space, size)
    def set_storage_for_list(self, space, w_obj, collection):
        w_obj.int_storage = self.storage_for_list(space, collection)
    def set_storage_copied_from(self, space, w_obj, w_source_obj, reuse_storage=False):
        w_obj.int_storage = self.copy_storage_from(space, w_source_obj, reuse_storage)
    
    def generelized_strategy_for(self, space, w_val):
        return ListStorageStrategy.singleton
    def initial_storage(self, space, size):
        raise NotImplementedError("Abstract base class")
    def storage_for_list(self, space, collection):
        raise NotImplementedError("Abstract base class")
    def copy_storage_from(self, space, w_obj, reuse_storage):
        old_strategy = w_obj.strategy
        if old_strategy == self and reuse_storage:
            return self.storage(w_obj)
        else:
            # This can be overridden and optimized (reuse_storage flag, less temporary storage)
            return self.storage_for_list(space, w_obj.fetch_all(space))

class SingletonMeta(type):
    def __new__(cls, name, bases, dct):
        result = type.__new__(cls, name, bases, dct)
        result.singleton = result()
        return result

# this is the typical "initial" storage strategy, for when every slot
# in an object is still nil. No storage is allocated.
class AllNilStorageStrategy(AbstractStorageStrategy):
    __metaclass__ = SingletonMeta
    strategy_tag = 'allnil'
    
    def can_contain(self, space, w_obj):
        return w_obj == model.w_nil
    def fetch(self, space, w_obj, n0):
        return model.w_nil
    def do_store(self, space, w_obj, n0, w_val):
        pass
        
    def generelized_strategy_for(self, space, w_val):
        return find_strategy_for_objects(space, [w_val])
    def set_initial_storage(self, space, w_obj, size):
        pass
    def set_storage_for_list(self, space, w_obj, collection):
        pass
    def set_storage_copied_from(self, space, w_obj, w_source_obj, reuse_storage=False):
        pass

# This is the regular storage strategy that does not result in any
# optimizations but can handle every case. Applicable for both
# fixed-sized and var-sized objects.
class ListStorageStrategy(AbstractListStorageStrategy):
    __metaclass__ = SingletonMeta
    strategy_tag = 'list'
    
    def can_contain(self, space, w_val):
        return True
    def fetch(self, space, w_obj, n0):
        return self.storage(w_obj)[n0]
    def do_store(self, space, w_obj, n0, w_val):
        # TODO enable generalization by maintaining a counter of elements that are nil.
        self.storage(w_obj)[n0] = w_val
    def initial_storage(self, space, size):
        return [model.w_nil] * size
    def storage_for_list(self, space, collection):
        return [x for x in collection]
    def copy_storage_from(self, space, w_obj, reuse_storage=False):
        length = w_obj.basic_size()
        return [w_obj.strategy.fetch(space, w_obj, i) for i in range(length)]

class AbstractValueOrNilStorageStrategy(AbstractIntStorageStrategy):
    needs_objspace = True
    strategy_tag = 'abstract-valueOrNil'
    # TODO -- use another value... something like max_float?
    nil_value = runpack("d", "\x10\x00\x00\x00\x00\x00\xf8\x7f")
    nil_value_longlong = longlong2float.float2longlong(nil_value)
    
    def is_nil_value(self, val):
        return longlong2float.float2longlong(val) == self.nil_value_longlong
    
    def can_contain(self, space, w_val):
        return w_val == model.w_nil or \
                (isinstance(w_val, self.wrapper_class) \
                and not self.is_nil_value(self.unwrap(space, w_val)))
    
    def fetch(self, space, w_obj, n0):
        val = self.storage(w_obj)[n0]
        if self.is_nil_value(val):
            return space.w_nil
        else:
            return self.wrap(space, val)
        
    def do_store(self, space, w_obj, n0, w_val):
        store = self.storage(w_obj)
        if w_val == model.w_nil:
            store[n0] = self.nil_value
        else:
            store[n0] = self.unwrap(space, w_val)
    
    def initial_storage(self, space, size):
        return [self.nil_value] * size
        
    def storage_for_list(self, space, collection):
        length = len(collection)
        store = self.initial_storage(space, length)
        for i in range(length):
            if collection[i] != model.w_nil:
                store[i] = self.unwrap(space, collection[i])
        return store

def _int_to_float(int_val):
    return longlong2float.longlong2float(rffi.cast(lltype.SignedLongLong, int_val))

class SmallIntegerOrNilStorageStrategy(AbstractValueOrNilStorageStrategy):
    __metaclass__ = SingletonMeta
    strategy_tag = 'smallint-orNil'
    wrapper_class = model.W_SmallInteger
    
    def wrap(self, space, val):
        int_val = rarithmetic.intmask(longlong2float.float2longlong(val))
        return space.wrap_int(int_val)
    def unwrap(self, space, w_val):
        assert isinstance(w_val, model.W_SmallInteger)
        int_val = space.unwrap_int(w_val)
        return _int_to_float(int_val)

class FloatOrNilStorageStrategy(AbstractValueOrNilStorageStrategy):
    __metaclass__ = SingletonMeta
    strategy_tag = 'float-orNil'
    wrapper_class = model.W_Float
    
    def wrap(self, space, val):
        return space.wrap_float(val)
    def unwrap(self, space, w_val):
        assert isinstance(w_val, model.W_Float)
        return space.unwrap_float(w_val)

def find_strategy_for_objects(space, vars):
    specialized_strategies = 3
    all_nil_can_handle = True
    small_int_can_handle = True
    float_can_handle = True
    for w_obj in vars:
        if all_nil_can_handle and not AllNilStorageStrategy.singleton.can_contain(space, w_obj):
            all_nil_can_handle = False
            specialized_strategies = specialized_strategies - 1
        if small_int_can_handle and not SmallIntegerOrNilStorageStrategy.singleton.can_contain(space, w_obj):
            small_int_can_handle = False
            specialized_strategies = specialized_strategies - 1
        if float_can_handle and not FloatOrNilStorageStrategy.singleton.can_contain(space, w_obj):
            float_can_handle = False
            specialized_strategies = specialized_strategies - 1
        
        if specialized_strategies <= 0:
            return ListStorageStrategy.singleton
    
    if all_nil_can_handle:
        return AllNilStorageStrategy.singleton
    if small_int_can_handle:
        return SmallIntegerOrNilStorageStrategy.singleton
    if float_can_handle:
        return FloatOrNilStorageStrategy.singleton
    
    # If this happens, please look for a bug in the code above.
    assert False, "No strategy could be found for list..."

def empty_strategy(s_containing_class):
    if s_containing_class is None:
        # This is a weird and rare special case for w_nil
        return ListStorageStrategy.singleton
    if not s_containing_class.isvariable():
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
    
    if is_variable:
        return find_strategy_for_objects(s_containing_class.space, vars)
    else:
        return ListStorageStrategy.singleton
