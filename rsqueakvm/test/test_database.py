import platform
import py
import pytest

from rsqueakvm import storage_classes
from rsqueakvm.database import dbm
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.database import W_DBObject
from rsqueakvm.model.pointers import W_PointersObject

from .util import create_space


def _import_SQPyteDB():
    try:
        assert "64bit" in platform.architecture()[0]
        from sqpyte.interpreter import SQPyteDB
        return SQPyteDB
    except (ImportError, AssertionError):
        return None

SQPyteDB = _import_SQPyteDB()

skipif_incompatible = pytest.mark.skipif(
    SQPyteDB is None, reason="64bit required or sqpyte not found")


def _bootstrap_class(instsize, name, w_superclass=None, w_metaclass=None,
                     format=storage_classes.POINTERS, varsized=True):
    space = create_space(bootstrap=True)
    return space.bootstrap_class(instsize, w_superclass, w_metaclass,
                                 name, format, varsized)


@skipif_incompatible
def test_dbmanager_primitives(monkeypatch):
    py.test.raises(PrimitiveFailedError, lambda: dbm.get_connection('foo'))
    db_handle = dbm.connect(SQPyteDB, 'test.db')
    assert db_handle == 0
    db = dbm.get_connection(db_handle)
    cursor_handle = dbm.execute(None, db, 'SELECT 1;')
    assert cursor_handle == 0
    monkeypatch.undo()


@skipif_incompatible
def test_dbmanager_db_object():
    conn1 = dbm.connection()
    conn2 = dbm.connection()
    assert conn1 is conn2


@skipif_incompatible
def test_dbobject_fetch_and_store_with_string():
    space = create_space(bootstrap=True)
    instsize = 10
    obj = W_DBObject(space, _bootstrap_class(instsize, "Foo"), instsize)
    val = "test-string"
    obj.store(space, 0, space.wrap_string(val))
    returned_val = obj.fetch(space, 0)

    assert val == space.unwrap_string(returned_val)


@skipif_incompatible
def test_dbobject_fetch_and_store_with_pointers_object():
    space = create_space(bootstrap=True)
    instsize = 10
    obj = W_DBObject(space, _bootstrap_class(instsize, "Foo"), instsize)
    val = W_PointersObject(space, _bootstrap_class(instsize, "Foo"), instsize)
    obj.store(space, 0, val)
    returned_val = obj.fetch(space, 0)

    assert val == returned_val


@skipif_incompatible
def test_dbobject_fetch_and_store_with_w_db_object():
    space = create_space(bootstrap=True)
    instsize = 10
    obj = W_DBObject(space, _bootstrap_class(instsize, "Doo"), instsize)
    val = W_DBObject(space, _bootstrap_class(instsize, "Bar"), instsize)
    obj.store(space, 0, val)
    returned_val = obj.fetch(space, 0)

    assert val == returned_val
