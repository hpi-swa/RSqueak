# -*- coding: utf-8 -*-

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.primitives import expose_primitive
from rsqueakvm.primitives.bytecodes import *

# from rpython.rlib import jit
from rpython.rtyper.lltypesystem import rffi

from sqpyte.capi import CConfig
from sqpyte.interpreter import Sqlite3DB, SQPyteException, SqliteException


class _SQPyteDB(object):

    def __init__(self, filename):
        self.connect(filename)

    def execute(self, sql):
        # jit.promote(sql)
        return self._db.execute(sql)

    def connect(self, filename):
        # Open database
        try:
            print "Trying to connect to %s..." % filename
            self._db = Sqlite3DB(filename)
            print "Success"
        except (SQPyteException, SqliteException) as e:
            print e

    def disconnected(self):
        return self._db is None

    def close(self):
        if self.disconnected():
            return False

        self._db.close()
        self._db = None
        print "Disconnected"
        return True


class _SQPyteCursor(object):
    def __init__(self, cursor):
        self.cursor = cursor
        self.num_cols = 0
        self.rc = 0

    def next(self):
        self.rc = self.cursor.mainloop()

        if not self.num_cols:
            self.num_cols = self.cursor.data_count()

        if self.rc != CConfig.SQLITE_ROW:
            return None

        return self.cursor


class _DBManager(object):
    _db_count = 0
    _dbs = {}
    _cursor_count = 0
    _cursors = {}

    def __init__(self):
        pass

    def connect(self, filename):
        pointer = self._db_count
        self._dbs[pointer] = _SQPyteDB(filename)

        self._db_count += 1

        return pointer

    def execute(self, db_pointer, sql_statement):
        db = self._dbs.get(db_pointer, None)
        if not db:
            raise PrimitiveFailedError

        pointer = self._cursor_count
        self._cursors[pointer] = _SQPyteCursor(db.execute(sql_statement))

        self._cursor_count += 1

        return pointer

    def cursor(self, cursor_pointer):
        return self._cursors.get(cursor_pointer, None)

    def close(self, db_pointer):
        db = self._dbs.get(db_pointer, None)
        if not db:
            raise PrimitiveFailedError
        return db.close()


dbm = _DBManager()


@expose_primitive(SQPYTE_CONNECT, unwrap_spec=[object, str])
def sqpyte_connect(interp, s_frame, w_rcvr, filename):
    return interp.space.wrap_int(dbm.connect(filename))


@expose_primitive(SQPYTE_EXECUTE, unwrap_spec=[object, int, str])
def sqpyte_execute(interp, s_frame, w_rcvr, db_pointer, sql_statement):
    return interp.space.wrap_int(dbm.execute(db_pointer, sql_statement))


@expose_primitive(SQPYTE_NEXT, unwrap_spec=[object, int])
def sqpyte_next(interp, s_frame, w_rcvr, cursor_pointer):
    cursor = dbm.cursor(cursor_pointer)
    if not cursor:
        return interp.space.w_nil
    row = fetch_one_row(cursor, interp.space)
    return interp.space.wrap_list(row)


def fetch_one_row(cursor, space):
    query = cursor.next()
    cols = [None] * cursor.num_cols
    for i in range(cursor.num_cols):
        typ = query.column_type(i)
        if typ == CConfig.SQLITE_TEXT or typ == CConfig.SQLITE_BLOB:
            textlen = query.column_bytes(i)
            result = rffi.charpsize2str(rffi.cast(rffi.CCHARP,
                                                  query.column_text(i)),
                                        textlen)
            w_result = space.wrap_string(result)  # no encoding
        elif typ == CConfig.SQLITE_INTEGER:
            result = query.column_int64(i)
            w_result = space.wrap_int(result)
        elif typ == CConfig.SQLITE_FLOAT:
            result = query.column_double(i)
            w_result = space.wrap_float(result)
        elif typ == CConfig.SQLITE_NULL:
            w_result = space.w_nil
        else:
            raise PrimitiveFailedError
        cols[i] = w_result
    return cols


@expose_primitive(SQPYTE_CLOSE, unwrap_spec=[object, int])
def sqpyte_close(interp, s_frame, w_rcvr, db_pointer):
    return interp.space.wrap_bool(dbm.close(db_pointer))


###############################################################################
# Interpreter-only, because sqlite3 cannot be compiled with RPython ¯\_(ツ)_/¯ #
###############################################################################
# from rpython.rlib import objectmodel
# if objectmodel.we_are_translated():
#     import sqlite3

#     @expose_primitive(SQLITE, unwrap_spec=[object, str, str])
#     def func(interp, s_frame, w_rcvr, db_file, sql_statement):

#         print db_file
#         print sql_statement

#         conn = sqlite3.connect(db_file)
#         try:
#             cursor = conn.cursor()

#             cursor.execute(sql_statement)
#             result = [str('; '.join(row)) for row in cursor]
#         finally:
#             conn.close()

#         return interp.space.wrap_string('%s' % '\n '.join(result))
###############################################################################
