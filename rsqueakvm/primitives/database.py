# -*- coding: utf-8 -*-

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.primitives import expose_primitive
from rsqueakvm.primitives.bytecodes import *

from rpython.rlib import jit
from rpython.rtyper.lltypesystem import rffi

from sqpyte.capi import CConfig
from sqpyte.interpreter import Sqlite3DB, SQPyteException, SqliteException


class Statement(object):
    _immutable_fields_ = ['w_connection', 'sql', 'query']

    def __init__(self, w_connection, sql):
        assert isinstance(w_connection, _SQPyteDB)
        self.w_connection = w_connection
        self.sql = sql
        try:
            self.query = w_connection.db.execute(sql)
        except SqliteException, e:
            print e.msg
            raise PrimitiveFailedError
            # space = w_connection.space
            # w_module = space.getbuiltinmodule('sqpyte')
            # w_error = space.getattr(w_module, space.wrap('OperationalError'))
            # raise OperationError(w_error, space.wrap(e.msg))
        # self.query.use_translated.disable_from_cmdline(
        #     w_connection.disable_opcodes)

    def close(self):
        if self.query:
            self.query.close()
            self.query = None

    def _reset(self):
        cache = self.w_connection.statement_cache
        holder = cache.get_holder(self.sql)
        if holder.statement is not None:
            self.close()
        else:
            holder.statement = self
            self.query.reset_query()


class StatementHolder(object):
    def __init__(self):
        self.statement = None

    def _get_or_make(self, cache, sql):
        if self.statement is None:
            return Statement(cache.w_connection, sql)
        result = self.statement
        self.statement = None
        return jit.promote(result)


class StatementCache(object):
    def __init__(self, w_connection):
        self.w_connection = w_connection
        self.cache = {}

    def get_or_make(self, sql):
        holder = self.get_holder(sql)
        return holder._get_or_make(self, sql)

    def get_holder(self, sql):
        jit.promote(self)
        return self._get_holder(sql)

    @jit.elidable
    def _get_holder(self, sql):
        holder = self.cache.get(sql, None)
        if not holder:
            holder = self.cache[sql] = StatementHolder()
        return holder

    def all_statements(self):
        # return [holder.statement for holder in self.cache.itervalues()
        #         if holder.statement is not None]
        return []


class _SQPyteDB(object):
    _immutable_fields_ = ['db']

    def __init__(self, filename):
        self.connect(filename)
        self.statement_cache = StatementCache(self)
        self.is_closed = False

    def execute(self, sql):
        statement = self.statement_cache.get_or_make(sql)
        return _SQPyteCursor(statement)

    def connect(self, filename):
        # Open database
        try:
            print "Trying to connect to %s..." % filename
            self.db = Sqlite3DB(filename)
            print "Success"
        except (SQPyteException, SqliteException) as e:
            print e.msg

    def close(self):
        if self.is_closed:
            return False

        for holder in self.statement_cache.all_statements():
            holder.close()
        self.db.close()
        self.is_closed = True
        print "Disconnected"
        return True


class _SQPyteCursor(object):
    _immutable_fields_ = ['statement']

    def __init__(self, statement):
        self.statement = statement
        if not self.statement.query:
            self.rc = 0
            self.num_cols = 0
        else:
            self.rc = self.statement.query.mainloop()
            self.num_cols = self.statement.query.data_count()

    def next(self, space):
        if self.rc != CConfig.SQLITE_ROW:
            return None
        else:
            row = self.fetch_one_row(space)
            self.rc = self.statement.query.mainloop()
            return row

    @jit.unroll_safe
    def fetch_one_row(self, space):
        query = jit.promote(self.statement).query
        cols = [None] * self.num_cols
        for i in range(self.num_cols):
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

    def execute(self, db_pointer, sql):
        db = self._dbs.get(db_pointer, None)
        if not db:
            raise PrimitiveFailedError

        pointer = self._cursor_count

        statement = db.statement_cache.get_or_make(sql)
        self._cursors[pointer] = _SQPyteCursor(statement)

        self._cursor_count += 1

        return pointer

    @jit.elidable
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
def sqpyte_execute(interp, s_frame, w_rcvr, db_pointer, sql):
    return interp.space.wrap_int(dbm.execute(db_pointer, sql))


@expose_primitive(SQPYTE_NEXT, unwrap_spec=[object, int])
def sqpyte_next(interp, s_frame, w_rcvr, cursor_pointer):
    cursor = dbm.cursor(cursor_pointer)
    row = cursor.next(interp.space)
    if row:
        return interp.space.wrap_list(row)
    # cursor exhausted
    return interp.space.w_nil


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
#     def func(interp, s_frame, w_rcvr, db_file, sql):

#         print db_file
#         print sql

#         conn = sqlite3.connect(db_file)
#         try:
#             cursor = conn.cursor()

#             cursor.execute(sql)
#             result = [str('; '.join(row)) for row in cursor]
#         finally:
#             conn.close()

#         return interp.space.wrap_string('%s' % '\n '.join(result))
###############################################################################
