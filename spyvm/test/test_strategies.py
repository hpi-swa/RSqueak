import py
from spyvm import wrapper, model, interpreter, objspace, strategies
from spyvm.model import w_nil
from spyvm.test import test_miniimage as tools
from spyvm.error import WrapperException, FatalError

# Fieldtypes have a separate test file

space, interp = tools.setup_module(tools, filename='bootstrapped.image')
class_Array = space.classtable["w_Array"]

def arr(size):
    return model.W_PointersObject(space, class_Array, size)

def list_arr(size):
    a = arr(size)
    a.store(space, 0, arr(1))
    return a

def tagging_arr(size):
    a = arr(size)
    a.store(space, 0, space.wrap_int(12))
    return a

def tagging_arr_odd(size):
    a = arr(size)
    a.store(space, 2, space.wrap_int(12))
    return a

def check_arr(arr, expected):
    for i in range(arr.basic_size()):
        if expected[i] == w_nil:
            assert arr.fetch(space, i) == w_nil
        else:
            w_val = arr.fetch(space, i)
            assert isinstance(w_val, model.W_SmallInteger)
            assert space.unwrap_int(w_val) == expected[i]

# ====== AllNil StorageStrategy

def test_EmptyArray():
    a = arr(5)
    assert isinstance(a.strategy, strategies.AllNilStorageStrategy)

def test_StoreNil():
    a = arr(5)
    a.store(space, 0, w_nil)
    a.store(space, 4, w_nil)
    assert isinstance(a.strategy, strategies.AllNilStorageStrategy)

def test_FetchNil():
    a = arr(5)
    assert a.fetch(space, 2) is w_nil

def test_AllNilSize():
    a = arr(5)
    assert a.basic_size() == 5

# ====== List StorageStrategy

def test_AllNil_to_List():
    a = list_arr(5)
    assert isinstance(a.strategy, strategies.ListStorageStrategy)

def test_List_store():
    a = list_arr(5)
    a.store(space, 1, arr(1))
    a.store(space, 4, arr(1))
    assert isinstance(a.strategy, strategies.ListStorageStrategy)

def test_List_fetch():
    a = list_arr(5)
    assert a.fetch(space, 0).getclass(space) == class_Array
    assert a.fetch(space, 4) == w_nil

def test_List_size():
    a = list_arr(5)
    a.store(space, 1, arr(1))
    assert a.basic_size() == 5

# ====== Tagging SmallInteger StorageStrategy

def test_AllNil_to_Int():
    a = tagging_arr(5)
    assert isinstance(a.strategy, strategies.TaggingSmallIntegerStorageStrategy)
    check_arr(a, [12, w_nil, w_nil, w_nil, w_nil])

def test_Tagging_store():
    a = tagging_arr(5)
    a.store(space, 1, space.wrap_int(20))
    a.store(space, 2, space.wrap_int(20))
    assert isinstance(a.strategy, strategies.TaggingSmallIntegerStorageStrategy)
    check_arr(a, [12, 20, 20, w_nil, w_nil])

def test_Tagging_store_nil_to_nil():
    a = tagging_arr_odd(5)
    a.store(space, 1, w_nil)
    check_arr(a, [w_nil, w_nil, 12, w_nil, w_nil])
    
def test_Tagging_delete():
    a = tagging_arr_odd(5)
    a.store(space, 1, space.wrap_int(1))
    a.store(space, 3, space.wrap_int(2))
    a.store(space, 2, space.wrap_int(100))
    a.store(space, 1, space.wrap_int(200))
    a.store(space, 3, space.wrap_int(300))
    check_arr(a, [w_nil, 200, 100, 300, w_nil])

def test_Tagging_delete_first():
    a = tagging_arr_odd(5)
    a.store(space, 1, space.wrap_int(1))
    a.store(space, 1, w_nil)
    check_arr(a, [w_nil, w_nil, 12, w_nil, w_nil])

def test_Tagging_to_List():
    a = tagging_arr_odd(5)
    a.store(space, 1, arr(1))
    assert isinstance(a.strategy, strategies.ListStorageStrategy)
