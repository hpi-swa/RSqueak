# -*- coding: utf-8 -*-

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.primitives.bytecodes import *

from rpython.rlib import jit
from rpython.rtyper.lltypesystem import rffi, lltype

from sqpyte import capi
from sqpyte.capi import CConfig
from sqpyte.interpreter import Sqlite3DB as SQPyteDBBase, SQPyteException, SqliteException

# These empty base classes are for some reason necessary for RPython.
# object doesn't suffice as a common base class.
class DBClassBase(object):
    def __init__(self):
        pass

class DBCursorBase(object):
    def __init__(self):
        pass

class SQPyteDB(DBClassBase):

    def __init__(self, filename, space):
        self.sqpyte = SQPyteDBBase(filename)

    def cursor(self, connection):
        return _SQPyteCursor(connection)

    def close(self):
        pass

    def execute(self, sql):
        return self.sqpyte.execute(sql)

DatabasePlugin = Plugin()

class Statement(object):
    _immutable_fields_ = ['w_connection', 'sql', 'query']

    def __init__(self, w_connection, sql):
        assert isinstance(w_connection, DatabaseWrapper)
        self.w_connection = w_connection
        self.sql = sql
        try:
            self.query = w_connection.db.execute(sql)
        except SqliteException, e:
            print e.msg
            raise PrimitiveFailedError(e.msg)
            # space = w_connection.space
            # w_module = space.getbuiltinmodule('sqpyte')
            # w_error = space.getattr(w_module, space.wrap('OperationalError'))
            # raise PrimitiveFailedError(w_error, space.wrap(e.msg))
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


class DatabaseWrapper(object):
    _immutable_fields_ = ['db', 'statement_cache']

    def __init__(self, space, filename, DBClass):
        self.space = space
        self.dbClass = DBClass
        self.statement_cache = StatementCache(self)
        self.is_closed = False

        self.connect(filename)

    def cursor(self):
        return self.db.cursor(self)

    def execute(self, sql, args, db_pointer):
        if self.db is None:
            raise PrimitiveFailedError('db is closed')
        return self.cursor().execute(sql, args, db_pointer)

    def connect(self, filename):
        # Open database
        try:
            print 'Trying to connect to %s...' % filename
            self.db = self.dbClass(filename, self.space)
            print 'Success'
        except (SQPyteException, SqliteException) as e:
            print e.msg

    def close(self):
        if self.is_closed:
            return False

        for holder in self.statement_cache.all_statements():
            holder.close()
        self.db.close()
        self.is_closed = True
        print 'Disconnected'
        return True


class _SQPyteCursor(DBCursorBase):
    _immutable_fields_ = ['connection']

    def __init__(self, connection):
        self.space = connection.space
        assert isinstance(connection, DatabaseWrapper)
        self.connection = connection
        self.statement = None

    @jit.unroll_safe
    def execute(self, sql, args, db_pointer):
        jit.promote(self.connection)
        jit.promote(self.statement)
        cache = self.connection.statement_cache
        self.statement = cache.get_or_make(sql)

        if args is not None:
            query = self.statement.query
            if len(args) != query.bind_parameter_count():
                raise PrimitiveFailedError('wrong # of arguments for query')
            for i, w_value in enumerate(args):
                self._convert_query_argument(w_value, query, i + 1)

        rc = self.statement.query.mainloop()
        if rc == CConfig.SQLITE_ROW:
            pass  # storage stays on sqlite side
        elif rc == CConfig.SQLITE_DONE:
            self._reset()
        else:
            raise PrimitiveFailedError('strange result: %s' % rc)
        return self

    def next(self):
        if jit.promote(self.statement) is None:
            return self.space.w_nil

        w_row = self.fetch_one_row()
        rc = self.statement.query.mainloop()
        if rc == CConfig.SQLITE_ROW:
            pass
        elif rc == CConfig.SQLITE_DONE:
            self._reset()
        else:
            raise PrimitiveFailedError('strange result: %s' % rc)
        return self.space.wrap_list(w_row)

    @jit.unroll_safe
    def fetch_one_row(self):
        query = jit.promote(self.statement).query
        num_cols = query.data_count()
        jit.promote(num_cols)
        cols = [None] * num_cols
        for i in range(num_cols):
            tid = query.column_type(i)
            if tid == CConfig.SQLITE_TEXT or tid == CConfig.SQLITE_BLOB:
                textlen = query.column_bytes(i)
                result = rffi.charpsize2str(rffi.cast(rffi.CCHARP,
                                                      query.column_text(i)),
                                            textlen)
                w_result = self.space.wrap_string(result)  # no encoding
            elif tid == CConfig.SQLITE_INTEGER:
                result = query.column_int64(i)
                w_result = self.space.wrap_int(result)
            elif tid == CConfig.SQLITE_FLOAT:
                result = query.column_double(i)
                w_result = self.space.wrap_float(result)
            elif tid == CConfig.SQLITE_NULL:
                w_result = self.space.w_nil
            else:
                raise PrimitiveFailedError('read_row [tid: %s' % tid)
            cols[i] = w_result
        return cols

    def close(self):
        if self.statement:
            self.statement.close()
            self.statement = None

    def _convert_query_argument(self, w_value, query, i):
        space = self.space
        cls = w_value.getclass(space)
        if (cls.is_same_object(space.w_String)):
            query.bind_str(i, space.unwrap_string(w_value))
        elif cls.is_same_object(space.w_SmallInteger):
            query.bind_int64(i, space.unwrap_int(w_value))
        elif cls.is_same_object(space.w_Float):
            query.bind_double(i, space.unwrap_float(w_value))
        elif cls.is_same_object(space.w_nil):
            query.bind_null(i)
        else:
            raise PrimitiveFailedError(
                'unable to unwrap %s' % w_value.getclass(space))

    def _reset(self):
        if self.statement:
            self.statement._reset()
            self.statement = None


class _DBManager(object):
    _db_count = 0
    _dbs = {}
    _cursor_count = 0
    _cursors = {}

    def __init__(self):
        pass

    def connect(self, space, filename, DBClass):
        pointer = self._db_count
        self._dbs[pointer] = DatabaseWrapper(space, filename, DBClass)

        self._db_count += 1

        return pointer

    def execute(self, db_pointer, sql, args):
        db = self._dbs.get(db_pointer, None)
        if db is None:
            raise PrimitiveFailedError('execute [db is None]')

        pointer = self._cursor_count
        self._cursors[pointer] = db.execute(sql, args, db_pointer)

        self._cursor_count += 1

        return pointer

    @jit.elidable
    def cursor(self, cursor_pointer):
        return self._cursors.get(cursor_pointer, None)

    def close(self, db_pointer):
        db = self._dbs.get(db_pointer, None)
        if db is None:
            raise PrimitiveFailedError('close [db is None]')
        return db.close()


dbm = _DBManager()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, str])
def primitiveSQPyteConnect(interp, s_frame, w_rcvr, filename):
    return interp.space.wrap_int(dbm.connect(interp.space, filename, SQPyteDB))


@DatabasePlugin.expose_primitive(clean_stack=False)
def primitiveSQPyteExecute(interp, s_frame, argcount):
    if not 2 <= argcount <= 3:
        raise PrimitiveFailedError('wrong number of arguments: %s' % argcount)

    space = interp.space

    args = []
    if argcount == 3:
        args = space.unwrap_array(s_frame.pop())

    arg3_w = s_frame.pop()
    sql = space.unwrap_string(arg3_w)
    arg2_w = s_frame.pop()
    db_pointer = space.unwrap_longlong(arg2_w)
    s_frame.pop()  # receiver

    return interp.space.wrap_int(dbm.execute(db_pointer, sql, args))


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQPyteNext(interp, s_frame, w_rcvr, cursor_pointer):
    cursor = dbm.cursor(cursor_pointer)
    if cursor is None:
        raise PrimitiveFailedError('cursor not found')
    return cursor.next()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQPyteClose(interp, s_frame, w_rcvr, db_pointer):
    return interp.space.wrap_bool(dbm.close(db_pointer))


#
# libsqlite3 via rffi
#
sqlite3_step = capi.llexternal('sqlite3_step', [capi.VDBEP], rffi.INT)
sqlite3_column_count = capi.llexternal('sqlite3_column_count', [capi.VDBEP],
                                       rffi.INT)

class SQLiteDB(DBClassBase):

    def __init__(self, filename, space):
        self.space = space
        self.open(filename)

    def open(self, filename):
        with rffi.scoped_str2charp(filename) as filename, \
                lltype.scoped_alloc(capi.SQLITE3PP.TO, 1) as result:
            rc = capi.sqlite3_open(filename, result)

            if rc == CConfig.SQLITE_OK:
                self.ptr = result[0]
            else:
                raise PrimitiveFailedError('conntect [rc: %s]' % rc)

    def close(self):
        pass

    def cursor(self, connection):
        return _SQLiteCursor(connection)


class _SQLiteCursor(DBCursorBase):

    def __init__(self, connection):
        self.space = connection.space
        assert isinstance(connection, DatabaseWrapper)
        self.connection = connection
        self.statement = None
        self.isDone = False

    def execute(self, sql, args, db_pointer):

        length = len(sql)
        v_db_ptr = self.connection.db.ptr

        with rffi.scoped_str2charp(sql) as query_p, \
                lltype.scoped_alloc(rffi.VOIDPP.TO, 1) as result, \
                lltype.scoped_alloc(rffi.CCHARPP.TO, 1) as unused_buffer:
            errorcode = capi.sqlite3_prepare_v2(v_db_ptr, query_p, length, result,
                                                unused_buffer)
            if not errorcode == 0:
                raise PrimitiveFailedError('errorcode != 0: %s' % str(errorcode))

            v_pointer = rffi.cast(capi.VDBEP, result[0])
            self.ptr = result[0]

            if len(args) != capi.sqlite3_bind_parameter_count(v_pointer):
                raise PrimitiveFailedError('wrong # of arguments for query')

            for i, w_value in enumerate(args):
                _convert_query_argument(self.space, w_value, v_pointer, i + 1)

            rc = sqlite3_step(v_pointer)
            if rc != CConfig.SQLITE_ROW and rc != CConfig.SQLITE_DONE:
                raise PrimitiveFailedError('strange result: %s' % rc)

        return self

    def next(self):
        if self.isDone:
            return self.space.w_nil

        ptr = rffi.cast(capi.VDBEP, self.ptr)

        row = sqlite_read_row(self.space, ptr)
        rc = sqlite3_step(ptr)
        if rc == CConfig.SQLITE_ROW:
            pass
        elif rc == CConfig.SQLITE_DONE:
            self.isDone = True
        else:
            raise PrimitiveFailedError('next [rc: %s]' % rc)

        return self.space.wrap_list(row)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, str])
def primitiveSQLiteConnect(interp, s_frame, w_rcvr, filename):
    return interp.space.wrap_int(dbm.connect(interp.space, filename, SQLiteDB))

@DatabasePlugin.expose_primitive(clean_stack=False)
def primitiveSQLiteExecute(interp, s_frame, argcount):
    return primitiveSQPyteExecute(interp, s_frame, argcount)


def _convert_query_argument(space, w_value, v_pointer, i):
    cls = w_value.getclass(space)
    if (cls.is_same_object(space.w_String)):
        text = space.unwrap_string(w_value)
        charp = rffi.str2charp(text)
        capi.sqlite3_bind_text(v_pointer, i, charp, -1, None)
    elif cls.is_same_object(space.w_SmallInteger):
        capi.sqlite3_bind_int64(v_pointer, i, space.unwrap_int(w_value))
    elif cls.is_same_object(space.w_Float):
        capi.sqlite3_bind_double(v_pointer, i, space.unwrap_float(w_value))
    elif cls.is_same_object(space.w_nil):
        capi.sqlite3_bind_null(v_pointer, i)
    else:
        raise PrimitiveFailedError(
            'unable to unwrap %s' % w_value.getclass(space))


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLiteNext(interp, s_frame, w_rcvr, stmt_ptr):
    return primitiveSQPyteNext(interp, s_frame, w_rcvr, stmt_ptr)


def sqlite_read_row(space, ptr):
    column_count = sqlite3_column_count(ptr)
    row = [None] * column_count
    for i in range(column_count):
        tid = capi.sqlite3_column_type(ptr, i)
        if tid == CConfig.SQLITE_TEXT or tid == CConfig.SQLITE_BLOB:
            text_len = capi.sqlite3_column_bytes(ptr, i)
            text_ptr = capi.sqlite3_column_text(ptr, i)
            row[i] = space.wrap_string(
                rffi.charpsize2str(text_ptr, text_len))
        elif tid == CConfig.SQLITE_INTEGER:
            value = capi.sqlite3_column_int64(ptr, i)
            row[i] = space.wrap_int(value)
        elif tid == CConfig.SQLITE_FLOAT:
            value = capi.sqlite3_column_double(ptr, i)
            row[i] = space.wrap_float(value)

        elif tid == CConfig.SQLITE_NULL:
            row[i] = space.w_nil
        else:
            raise PrimitiveFailedError('read_row [tid: %s' % tid)
    return row


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLiteClose(interp, s_frame, w_rcvr, db_ptr):
    # TODO
    return interp.space.w_nil


###############################################################################
# Interpreter-only, because sqlite3 cannot be compiled with RPython ¯\_(ツ)_/¯ #
###############################################################################
# from rpython.rlib import objectmodel
# if objectmodel.we_are_translated():
#     import sqlite3

#     @DatabasePlugin.expose_primitive(SQLITE, unwrap_spec=[object, str, str])
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
