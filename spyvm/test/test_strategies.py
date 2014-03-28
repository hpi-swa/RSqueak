import py
from spyvm import wrapper, model, interpreter, shadow, storage_statistics
from spyvm.error import WrapperException, FatalError
from .util import read_image, copy_to_module, cleanup_module

def setup_module():
    space, interp, _, _ = read_image('bootstrapped.image')
    class_Array = space.classtable["w_Array"]
    w_nil = space.w_nil
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def arr(size):
    return model.W_PointersObject(space, class_Array, size)

def list_arr(size):
    a = arr(size)
    a.store(space, 0, arr(1))
    return a

def int_arr(size):
    a = arr(size)
    a.store(space, 0, space.wrap_int(12))
    return a

def float_arr(size):
    a = arr(size)
    a.store(space, 0, space.wrap_float(1.2))
    return a

def check_arr(arr, expected):
    for i in range(arr.size()):
        w_val = arr.fetch(space, i)
        if expected[i] == w_nil:
            assert w_val == w_nil
        elif isinstance(expected[i], int):
            assert isinstance(w_val, model.W_SmallInteger)
            assert space.unwrap_int(w_val) == expected[i]
        elif isinstance(expected[i], float):
            assert isinstance(w_val, model.W_Float)
            assert space.unwrap_float(w_val) == expected[i]
        else:
            assert False, "Unexpected array of expected values."

# ====== AllNil StorageShadow

def test_EmptyArray():
    a = arr(5)
    assert isinstance(a.shadow, shadow.AllNilStorageShadow)

def test_StoreNil():
    a = arr(5)
    a.store(space, 0, w_nil)
    a.store(space, 4, w_nil)
    assert isinstance(a.shadow, shadow.AllNilStorageShadow)

def test_FetchNil():
    a = arr(5)
    assert a.fetch(space, 2) is w_nil

def test_AllNilSize():
    a = arr(5)
    assert a.size() == 5

# ====== List StorageShadow

def test_AllNil_to_List():
    a = list_arr(5)
    assert isinstance(a.shadow, shadow.ListStorageShadow)

def test_List_store():
    a = list_arr(5)
    a.store(space, 1, arr(1))
    a.store(space, 4, arr(1))
    assert isinstance(a.shadow, shadow.ListStorageShadow)

def test_List_fetch():
    a = list_arr(5)
    assert a.fetch(space, 0).getclass(space) == class_Array
    assert a.fetch(space, 4) == w_nil

def test_List_size():
    a = list_arr(5)
    a.store(space, 1, arr(1))
    assert a.size() == 5

# ====== SmallIntegerOrNil StorageShadow

def test_AllNil_to_Int():
    a = int_arr(5)
    assert isinstance(a.shadow, shadow.SmallIntegerOrNilStorageShadow)
    check_arr(a, [12, w_nil, w_nil, w_nil, w_nil])

def test_SmallInt_store():
    a = int_arr(5)
    a.store(space, 1, space.wrap_int(20))
    a.store(space, 2, space.wrap_int(20))
    assert isinstance(a.shadow, shadow.SmallIntegerOrNilStorageShadow)
    check_arr(a, [12, 20, 20, w_nil, w_nil])

def test_SmallInt_store_nil_to_nil():
    a = int_arr(5)
    a.store(space, 1, w_nil)
    check_arr(a, [12, w_nil, w_nil, w_nil, w_nil])
    
def test_SmallInt_overwrite():
    a = int_arr(5)
    a.store(space, 1, space.wrap_int(1))
    a.store(space, 3, space.wrap_int(2))
    a.store(space, 0, space.wrap_int(100))
    a.store(space, 1, space.wrap_int(200))
    a.store(space, 3, space.wrap_int(300))
    check_arr(a, [100, 200, w_nil, 300, w_nil])

def test_SmallInt_delete():
    a = int_arr(5)
    a.store(space, 1, space.wrap_int(1))
    a.store(space, 1, w_nil)
    check_arr(a, [12, w_nil, w_nil, w_nil, w_nil])

def test_SmallInt_to_List():
    a = int_arr(5)
    a.store(space, 1, arr(1))
    assert isinstance(a.shadow, shadow.ListStorageShadow)

def test_SmallInt_store_Float_to_List():
    a = int_arr(5)
    a.store(space, 1, space.wrap_float(2.2))
    assert isinstance(a.shadow, shadow.ListStorageShadow)
    check_arr(a, [12, 2.2, w_nil, w_nil, w_nil])
    
# ====== FloatOrNil StorageShadow

def test_AllNil_to_Float():
    a = float_arr(5)
    assert isinstance(a.shadow, shadow.FloatOrNilStorageShadow)
    check_arr(a, [1.2, w_nil, w_nil, w_nil, w_nil])

def test_Float_store():
    a = float_arr(5)
    a.store(space, 1, space.wrap_float(20.0))
    a.store(space, 2, space.wrap_float(20.0))
    assert isinstance(a.shadow, shadow.FloatOrNilStorageShadow)
    check_arr(a, [1.2, 20.0, 20.0, w_nil, w_nil])

def test_Float_store_nil_to_nil():
    a = float_arr(5)
    a.store(space, 1, w_nil)
    check_arr(a, [1.2, w_nil, w_nil, w_nil, w_nil])
    
def test_Float_overwrite():
    a = float_arr(5)
    a.store(space, 1, space.wrap_float(1.0))
    a.store(space, 3, space.wrap_float(2.0))
    a.store(space, 0, space.wrap_float(100.0))
    a.store(space, 1, space.wrap_float(200.0))
    a.store(space, 3, space.wrap_float(300.0))
    check_arr(a, [100.0, 200.0, w_nil, 300.0, w_nil])

def test_Float_delete():
    a = float_arr(5)
    a.store(space, 1, space.wrap_float(1.0))
    a.store(space, 1, w_nil)
    check_arr(a, [1.2, w_nil, w_nil, w_nil, w_nil])

def test_Float_to_List():
    a = float_arr(5)
    a.store(space, 1, arr(1))
    assert isinstance(a.shadow, shadow.ListStorageShadow)

def test_Float_store_SmallInt_to_List():
    a = float_arr(5)
    a.store(space, 1, space.wrap_int(2))
    assert isinstance(a.shadow, shadow.ListStorageShadow)
    check_arr(a, [1.2, 2, w_nil, w_nil, w_nil])

def test_statistics_stats():
    stats = storage_statistics.StorageStatistics()
    stats.stat_operation(stats.make_key("B", "old", "new"), 3)
    stats.stat_operation(stats.make_key("B", "old", "new"), 4)
    stats.stat_operation(stats.make_key("B", "old2", "new2"), 20)
    stats.stat_operation(stats.make_key("B", "old", "new"), 5)
    stats.stat_operation(stats.make_key("A", "old", "new"), 1)
    stats.stat_operation(stats.make_key("A", "old", "new"), 2)
    stats.stat_operation(stats.make_key("C", "old", "new"), 10)
    stats.stat_operation(stats.make_key("C", "old", "new"), 11)
    keys = stats.sorted_keys()
    assert keys == [ ("A", "old", "new"), ("B", "old", "new"), ("B", "old2", "new2"), ("C", "old", "new") ]
    assert stats.stats[keys[0]] == [1, 2]
    assert stats.stats[keys[1]] == [3, 4, 5]
    assert stats.stats[keys[2]] == [20]
    assert stats.stats[keys[3]] == [10, 11]
    
def test_statistics_log():
    stats = storage_statistics.StorageStatistics()
    s = stats.log_operation_string(stats.make_key("Operation", "old_storage", "new_storage"), 22, "classname")
    assert s == "Operation (old_storage -> new_storage) of classname size 22"
    s = stats.log_operation_string(stats.make_key("InitialOperation", None, "some_new_storage"), 40, "a_classname")
    assert s == "InitialOperation (some_new_storage) of a_classname size 40"
    