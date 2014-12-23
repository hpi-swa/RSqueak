
from spyvm import model, storage
from .util import create_space_interp, copy_to_module, cleanup_module

def setup_module():
    space, interp = create_space_interp()
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
            assert w_val.is_nil(space)
        elif isinstance(expected[i], int):
            assert isinstance(w_val, model.W_SmallInteger)
            assert space.unwrap_int(w_val) == expected[i]
        elif isinstance(expected[i], float):
            assert isinstance(w_val, model.W_Float)
            assert space.unwrap_float(w_val) == expected[i]
        else:
            assert False, "Unexpected array of expected values."

# ====== StrategyFactory

def test_ordered_strategies():
    strategies = space.strategy_factory.strategies
    assert len(strategies) == 4
    index_nil = strategies.index(storage.AllNilStrategy)
    index_float = strategies.index(storage.FloatOrNilStrategy)
    index_int = strategies.index(storage.SmallIntegerOrNilStrategy)
    index_list = strategies.index(storage.ListStrategy)
    assert index_nil < index_float < index_list
    assert index_nil < index_int < index_list

def test_optimized_strategy_switch(monkeypatch):
    a = arr(5)
    def convert_storage_from(self, other):
        assert False, "The default convert_storage_from() routine should not be called!"
    
    monkeypatch.setattr(storage.AbstractStrategy, "convert_storage_from", convert_storage_from)
    try:
        s = a.strategy
        s.strategy_factory().switch_strategy(a, storage.SmallIntegerOrNilStrategy)
    finally:
        monkeypatch.undo()
    
# ====== AllNil Strategy

def test_EmptyArray():
    a = arr(5)
    assert isinstance(a.strategy, storage.AllNilStrategy)

def test_StoreNil():
    a = arr(5)
    a.store(space, 0, w_nil)
    a.store(space, 4, w_nil)
    assert isinstance(a.strategy, storage.AllNilStrategy)

def test_FetchNil():
    a = arr(5)
    assert a.fetch(space, 2) is w_nil

def test_AllNilSize():
    a = arr(5)
    assert a.size() == 5

# ====== List Strategy

def test_AllNil_to_List():
    a = list_arr(5)
    assert isinstance(a.strategy, storage.ListStrategy)

def test_List_store():
    a = list_arr(5)
    a.store(space, 1, arr(1))
    a.store(space, 4, arr(1))
    assert isinstance(a.strategy, storage.ListStrategy)

def test_List_fetch():
    a = list_arr(5)
    assert a.fetch(space, 0).getclass(space) == class_Array
    assert a.fetch(space, 4).is_nil(space)

def test_List_size():
    a = list_arr(5)
    a.store(space, 1, arr(1))
    assert a.size() == 5

# ====== SmallIntegerOrNil Strategy

def test_AllNil_to_Int():
    a = int_arr(5)
    assert isinstance(a.strategy, storage.SmallIntegerOrNilStrategy)
    check_arr(a, [12, w_nil, w_nil, w_nil, w_nil])

def test_SmallInt_store():
    a = int_arr(5)
    a.store(space, 1, space.wrap_int(20))
    a.store(space, 2, space.wrap_int(20))
    assert isinstance(a.strategy, storage.SmallIntegerOrNilStrategy)
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
    assert isinstance(a.strategy, storage.ListStrategy)

def test_SmallInt_store_Float_to_List():
    a = int_arr(5)
    a.store(space, 1, space.wrap_float(2.2))
    assert isinstance(a.strategy, storage.ListStrategy)
    check_arr(a, [12, 2.2, w_nil, w_nil, w_nil])
    
# ====== FloatOrNil Strategy

def test_AllNil_to_Float():
    a = float_arr(5)
    assert isinstance(a.strategy, storage.FloatOrNilStrategy)
    check_arr(a, [1.2, w_nil, w_nil, w_nil, w_nil])

def test_Float_store():
    a = float_arr(5)
    a.store(space, 1, space.wrap_float(20.0))
    a.store(space, 2, space.wrap_float(20.0))
    assert isinstance(a.strategy, storage.FloatOrNilStrategy)
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
    assert isinstance(a.strategy, storage.ListStrategy)

def test_Float_store_SmallInt_to_List():
    a = float_arr(5)
    a.store(space, 1, space.wrap_int(2))
    assert isinstance(a.strategy, storage.ListStrategy)
    check_arr(a, [1.2, 2, w_nil, w_nil, w_nil])
