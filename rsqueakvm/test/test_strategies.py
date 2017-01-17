from rsqueakvm import storage
from rsqueakvm.model.numeric import W_Float, W_SmallInteger
from rsqueakvm.model.pointers import W_PointersObject, W_FixedPointersObject

from .util import create_space_interp, copy_to_module, cleanup_module

def setup_module():
    space, interp = create_space_interp()
    class_Array = space.w_Array
    w_nil = space.w_nil
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def arr(size):
    return W_PointersObject(space, class_Array, size)

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

def pt(x, y):
    w_point = W_FixedPointersObject(space, space.w_Point, 2)
    w_point.store(interp.space, 0, space.w(x))
    w_point.store(interp.space, 1, space.w(y))
    return w_point

def check_arr(arr, expected):
    for i in range(arr.size()):
        w_val = arr.fetch(space, i)
        if expected[i] == w_nil:
            assert w_val.is_nil(space)
        elif expected[i] == None:
            assert w_val.is_nil(space)
        elif isinstance(expected[i], int):
            assert isinstance(w_val, W_SmallInteger)
            assert space.unwrap_int(w_val) == expected[i]
        elif isinstance(expected[i], float):
            assert isinstance(w_val, W_Float)
            assert space.unwrap_float(w_val) == expected[i]
        else:
            assert False, "Unexpected array of expected values."

# ====== StrategyFactory

def test_ordered_strategies():
    strategies = space.strategy_factory.strategies
    assert len(strategies) == 6
    index_nil = strategies.index(storage.AllNilStrategy)
    index_float = strategies.index(storage.FloatOrNilStrategy)
    index_int = strategies.index(storage.SmallIntegerOrNilStrategy)
    index_list = strategies.index(storage.ListStrategy)
    assert index_nil < index_float < index_list
    assert index_nil < index_int < index_list

def test_optimized_strategy_switch(monkeypatch):
    a = arr(5)
    def _convert_storage_from(self, other):
        assert False, "The default _convert_storage_from() routine should not be called!"

    monkeypatch.setattr(storage.AbstractStrategy, "_convert_storage_from", _convert_storage_from)
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

def test_maps_are_constant():
    a = pt(None, None)
    check_arr(a, [None, None])
    b = pt(None, None)
    check_arr(b, [None, None])
    assert a.strategy is b.strategy
    a.store(space, 1, space.wrap_int(2))
    b.store(space, 1, space.wrap_int(24))
    assert a.strategy is b.strategy
    a.store(space, 1, space.w_nil)
    assert a.strategy is not b.strategy
    b.store(space, 1, space.wrap_float(1.1))
    assert a.strategy is b.strategy
    a.store(space, 1, space.wrap_int(12))
    assert a.strategy is b.strategy

def test_store_SmallInt_uses_smallint_map():
    a = pt(None, None)
    check_arr(a, [None, None])
    assert isinstance(a.strategy, storage.MapStrategy)
    assert a.strategy.getprev() is None
    a.store(space, 1, space.wrap_int(2))
    check_arr(a, [None, 2])
    assert isinstance(a.strategy, storage.IntMapStorageNode)
    assert a.strategy.index == 1
    assert isinstance(a.strategy.prev, storage.MapStrategy)
    assert a.strategy.prev.getprev() is None

def test_object_map_does_not_change_to_int():
    a = pt(1.2, 2.2)
    check_arr(a, [1.2, 2.2])
    assert isinstance(a.strategy, storage.ObjectMapStorageNode)
    assert isinstance(a.strategy.prev, storage.ObjectMapStorageNode)
    assert isinstance(a.strategy.prev.prev, storage.MapStrategy)
    a.store(space, 1, space.wrap_int(2))
    check_arr(a, [1.2, 2])
    assert isinstance(a.strategy, storage.ObjectMapStorageNode)
    assert isinstance(a.strategy.prev, storage.ObjectMapStorageNode)
    assert isinstance(a.strategy.prev.prev, storage.MapStrategy)
    assert a.strategy.index == 1

def test_store_SmallInt_uses_sorted_smallint_map_then_removes_it():
    a = pt(None, None)
    a.store(space, 1, space.wrap_int(2))
    a.store(space, 0, space.wrap_int(3))
    check_arr(a, [3, 2])
    assert isinstance(a.strategy, storage.IntMapStorageNode)
    assert a.strategy.index == 1
    assert isinstance(a.strategy.prev, storage.IntMapStorageNode)
    assert a.strategy.prev.index == 0
    assert isinstance(a.strategy.prev.prev, storage.MapStrategy)
    assert a.strategy.prev.prev.getprev() is None
    a.store(space, 1, space.w_nil)
    assert isinstance(a.strategy, storage.ObjectMapStorageNode)
    assert a.strategy.index == 1
    assert isinstance(a.strategy.prev, storage.IntMapStorageNode)
    assert a.strategy.prev.index == 0
    assert isinstance(a.strategy.prev.prev, storage.MapStrategy)
    assert a.strategy.prev.prev.getprev() is None
    a.store(space, 1, space.wrap_int(2))
    assert isinstance(a.strategy, storage.ObjectMapStorageNode)
    assert a.strategy.index == 1
    assert isinstance(a.strategy.prev, storage.IntMapStorageNode)
    assert a.strategy.prev.index == 0
    assert isinstance(a.strategy.prev.prev, storage.MapStrategy)
    assert a.strategy.prev.prev.getprev() is None
    a.store(space, 1, space.wrap_float(1.1))
    assert isinstance(a.strategy, storage.ObjectMapStorageNode)
    assert a.strategy.index == 1
    assert isinstance(a.strategy.prev, storage.IntMapStorageNode)
    assert a.strategy.prev.index == 0
    assert a.strategy.prev.prev.getprev() is None
    a.store(space, 0, space.wrap_float(3.3))
    assert isinstance(a.strategy, storage.ObjectMapStorageNode)
    assert a.strategy.index == 1
    assert isinstance(a.strategy.prev, storage.ObjectMapStorageNode)
    assert a.strategy.prev.index == 0
    assert a.strategy.prev.prev.getprev() is None

def test_map_strategies_are_singletons():
    assert storage.MapStrategy._is_singleton
    for c in space.strategy_factory._collect_subclasses(storage.MapStrategy):
        assert c._is_singleton
