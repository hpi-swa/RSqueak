import platform
import py
import pytest

from rsqueakvm.database import dbm
from rsqueakvm.error import PrimitiveFailedError


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


@skipif_incompatible
def test_db_manager_primitives(monkeypatch):
    py.test.raises(PrimitiveFailedError, lambda: dbm.get_connection('foo'))
    db_handle = dbm.connect(SQPyteDB, 'test.db')
    assert db_handle == 0
    db = dbm.get_connection(db_handle)
    cursor_handle = dbm.execute(None, db, 'SELECT 1;')
    assert cursor_handle == 0
    monkeypatch.undo()


@skipif_incompatible
def test_db_manager_db_object():
    conn1 = dbm.connection()
    conn2 = dbm.connection()
    assert conn1 is conn2
