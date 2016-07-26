import py

from rsqueakvm.database import dbm, SQLCursor
from rsqueakvm.error import PrimitiveFailedError

from .util import create_space


class FakeDBDriver():

    def __init__(self, filename):
        self.filename = filename

    def execute(self, sql, use_flag_cache=True):
        pass

    def close(self):
        pass


def setup_module():
    space = create_space(bootstrap = True)


def test_db_manager_primitives(monkeypatch):
    monkeypatch.setattr(SQLCursor, "_step", lambda s: None)
    py.test.raises(PrimitiveFailedError, lambda: dbm.get_connection('foo'))
    db_handle = dbm.connect(FakeDBDriver, 'test.db')
    assert db_handle == 0
    db = dbm.get_connection(db_handle)
    cursor_handle = dbm.execute(None, db, 'sql statement')
    assert cursor_handle == 0
    monkeypatch.undo()


def test_db_manager_db_object():
    dbm.driver = FakeDBDriver
    conn1 = dbm.connection()
    conn2 = dbm.connection()
    assert conn1 is conn2
