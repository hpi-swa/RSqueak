import py
from spyvm import wrapper, model, interpreter, objspace, fieldtypes
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

def dense_arr(size):
    a = arr(size)
    a.store(space, 0, space.wrap_int(12))
    return a

def dense_arr_odd(size):
    a = arr(size)
    a.store(space, 2, space.wrap_int(12))
    return a

def sparse_arr(size):
    a = dense_arr(size)
    a.store(space, 2, space.wrap_int(20))
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
    assert isinstance(a.strategy, fieldtypes.AllNilStorageStrategy)

def test_StoreNil():
    a = arr(5)
    a.store(space, 0, w_nil)
    a.store(space, 4, w_nil)
    assert isinstance(a.strategy, fieldtypes.AllNilStorageStrategy)

def test_FetchNil():
    a = arr(5)
    assert a.fetch(space, 2) is w_nil

def test_AllNilSize():
    a = arr(5)
    assert a.basic_size() == 5

# ====== List StorageStrategy

def test_AllNil_to_List():
    a = list_arr(5)
    assert isinstance(a.strategy, fieldtypes.ListStorageStrategy)

def test_List_store():
    a = list_arr(5)
    a.store(space, 1, arr(1))
    a.store(space, 4, arr(1))
    assert isinstance(a.strategy, fieldtypes.ListStorageStrategy)

def test_List_fetch():
    a = list_arr(5)
    assert a.fetch(space, 0).getclass(space) == class_Array
    assert a.fetch(space, 4) == w_nil

def test_List_size():
    a = list_arr(5)
    a.store(space, 1, arr(1))
    assert a.basic_size() == 5

# ====== Dense and Sparse *SmallInteger-StorageStrategy

def test_AllNil_to_Dense():
    a = dense_arr(5)
    assert isinstance(a.strategy, fieldtypes.DenseSmallIntegerStorageStrategy)
    check_arr(a, [12, w_nil, w_nil, w_nil, w_nil])

def test_Dense_store():
    a = dense_arr(5)
    a.store(space, 1, space.wrap_int(20))
    a.store(space, 2, space.wrap_int(20))
    assert isinstance(a.strategy, fieldtypes.DenseSmallIntegerStorageStrategy)
    check_arr(a, [12, 20, 20, w_nil, w_nil])

def test_Dense_overwrite_middle():
    a = dense_arr(5)
    a.store(space, 1, space.wrap_int(20))
    a.store(space, 2, space.wrap_int(20))
    a.store(space, 1, space.wrap_int(30))
    check_arr(a, [12, 30, 20, w_nil, w_nil])

def test_Dense_overwrite_first():
    a = dense_arr(5)
    a.store(space, 1, space.wrap_int(20))
    a.store(space, 2, space.wrap_int(20))
    a.store(space, 0, space.wrap_int(30))
    check_arr(a, [30, 20, 20, w_nil, w_nil])

def test_Dense_overwrite_last():
    a = dense_arr(5)
    a.store(space, 1, space.wrap_int(20))
    a.store(space, 2, space.wrap_int(20))
    a.store(space, 2, space.wrap_int(30))
    check_arr(a, [12, 20, 30, w_nil, w_nil])

def test_Dense_odd():
    a = dense_arr_odd(5)
    assert isinstance(a.strategy, fieldtypes.DenseSmallIntegerStorageStrategy)
    check_arr(a, [w_nil, w_nil, 12, w_nil, w_nil])

def test_Dense_odd_store():
    a = dense_arr_odd(5)
    a.store(space, 1, space.wrap_int(20))
    a.store(space, 3, space.wrap_int(40))
    a.store(space, 4, space.wrap_int(30))
    check_arr(a, [w_nil, 20, 12, 40, 30])

def test_Dense_odd_overwrite():
    a = dense_arr_odd(5)
    a.store(space, 1, space.wrap_int(1))
    a.store(space, 3, space.wrap_int(2))
    a.store(space, 2, space.wrap_int(100))
    a.store(space, 1, space.wrap_int(200))
    a.store(space, 3, space.wrap_int(300))
    check_arr(a, [w_nil, 200, 100, 300, w_nil])

def test_Dense_store_nil_to_nil():
    a = dense_arr_odd(5)
    a.store(space, 1, w_nil)
    check_arr(a, [w_nil, w_nil, 12, w_nil, w_nil])
    
def test_Dense_delete():
    a = dense_arr_odd(5)
    a.store(space, 1, space.wrap_int(1))
    a.store(space, 3, space.wrap_int(2))
    a.store(space, 2, space.wrap_int(100))
    a.store(space, 1, space.wrap_int(200))
    a.store(space, 3, space.wrap_int(300))
    check_arr(a, [w_nil, 200, 100, 300, w_nil])

def test_Dense_delete_first():
    a = dense_arr_odd(5)
    a.store(space, 1, space.wrap_int(1))
    a.store(space, 1, w_nil)
    check_arr(a, [w_nil, w_nil, 12, w_nil, w_nil])

def test_Dense_delete_last():
    a = dense_arr_odd(5)
    a.store(space, 1, space.wrap_int(1))
    a.store(space, 2, w_nil)
    check_arr(a, [w_nil, 1, w_nil, w_nil, w_nil])

def test_Dense_to_AllNil():
    a = dense_arr_odd(5)
    a.store(space, 2, w_nil)
    assert isinstance(a.strategy, fieldtypes.AllNilStorageStrategy)

def test_Dense_to_List():
    a = dense_arr_odd(5)
    a.store(space, 1, arr(1))
    assert isinstance(a.strategy, fieldtypes.ListStorageStrategy)

def test_Dense_to_Sparse_by_deleting():
    a = dense_arr_odd(5)
    a.store(space, 1, space.wrap_int(10))
    a.store(space, 3, space.wrap_int(20))
    a.store(space, 2, w_nil)
    assert isinstance(a.strategy, fieldtypes.SparseSmallIntegerStorageStrategy)
    check_arr(a, [w_nil, 10, w_nil, 20, w_nil])

def test_Dense_to_Sparse_by_storing():
    a = dense_arr_odd(5)
    a.store(space, 4, space.wrap_int(10))
    assert isinstance(a.strategy, fieldtypes.SparseSmallIntegerStorageStrategy)
    check_arr(a, [w_nil, w_nil, 12, w_nil, 10])

def test_Sparse_store_nil():
    a = sparse_arr(5)
    a.store(space, 2, w_nil)
    check_arr(a, [12, w_nil, w_nil, w_nil, w_nil])

def test_Sparse_store():
    a = sparse_arr(5)
    a.store(space, 4, space.wrap_int(100))
    check_arr(a, [12, w_nil, 20, w_nil, 100])

def test_Sparse_to_List():
    a = sparse_arr(5)
    a.store(space, 4, arr(5))
    assert isinstance(a.strategy, fieldtypes.ListStorageStrategy)
