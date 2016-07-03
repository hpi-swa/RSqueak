# -*- coding: utf-8 -*-

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.plugin import Plugin, PluginStartupScripts
from rsqueakvm.primitives.bytecodes import *

from rpython.rlib import jit
from rpython.rtyper.lltypesystem import rffi

from sqpyte import interpreter
from sqpyte.capi import CConfig


DatabasePlugin = Plugin()


###############################################################################
# SQL Connection And Cursor Implementation                                    #
###############################################################################

class SQLConnection(object):
    _immutable_fields_ = ['db', 'statement_cache']

    def __init__(self, space, db_class, filename):
        self.space = space
        self.statement_cache = StatementCache(self)
        self.is_closed = False

        self.connect(db_class, filename)

    def connect(self, db_class, filename):
        try:
            print 'Trying to connect to %s...' % filename
            self.db = db_class(filename)
            print 'Success'
        except (interpreter.SQPyteException, interpreter.SqliteException) as e:
            print e.msg

    def cursor(self):
        return SQLCursor(self)

    def execute(self, sql, args=None):
        return self.cursor().execute(sql, args)

    def close(self):
        if self.is_closed:
            return False

        for holder in self.statement_cache.all_statements():
            holder.close()
        self.db.close()
        self.is_closed = True
        print 'Disconnected'
        return True


class SQLCursor(object):
    _immutable_fields_ = ['connection', 'space']

    def __init__(self, connection):
        self.space = connection.space
        assert isinstance(connection, SQLConnection)
        self.connection = connection
        self.statement = None

    @jit.unroll_safe
    def execute(self, sql, args):
        jit.promote(self.connection)
        jit.promote(self.statement)
        cache = self.connection.statement_cache
        self.statement = cache.get_or_make(sql)

        if args is not None:
            query = self.statement.query
            if len(args) != query.bind_parameter_count():
                raise PrimitiveFailedError('wrong # of arguments for query')
            for i, w_value in enumerate(args):
                self.bind_query_argument(w_value, query, i + 1)

        rc = self.statement.query.mainloop()
        if rc == CConfig.SQLITE_ROW:
            pass  # storage stays on sqlite side
        elif rc == CConfig.SQLITE_DONE:
            self._reset()
        else:
            raise PrimitiveFailedError('strange result: %s' % rc)
        return self

    def bind_query_argument(self, w_value, query, i):
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
        # This should be unroll safe, since fetch_one_row was also marked
        # unroll_safe.
        return self.space.wrap_list_unroll_safe(w_row)

    def column_count(self):
        column_count = self.statement.query.data_count()
        return self.space.wrap_int(column_count)

    def column_name(self, index):
        query = self.statement.query
        assert query is not None
        column_name = rffi.charp2strn(query.column_name(index), 255)
        return self.space.wrap_string(column_name)

    def column_names(self):
        names = []
        for i in range(0, self.statement.query.data_count()):
            names.append(self.column_name(i))

        return self.space.wrap_list(names)

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

    def _reset(self):
        if self.statement:
            self.statement._reset()
            self.statement = None


###############################################################################
# Statement Caching                                                           #
###############################################################################

class Statement(object):
    _immutable_fields_ = ['w_connection', 'sql', 'query']

    def __init__(self, w_connection, sql):
        assert isinstance(w_connection, SQLConnection)
        self.w_connection = w_connection
        self.sql = sql
        try:
            self.query = w_connection.db.execute(sql)
        except interpreter.SqliteException, e:
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

    def invalidate(self):
        self.cache = {}

###############################################################################
# Generic Database Manager                                                    #
###############################################################################


class DBManager(object):
    _immutable_fields_ = ["db_connection?"]

    def __init__(self):
        self.driver = interpreter.SQPyteDB  # Driver for DBObjects
        self.db_file_name = ":memory:"
        self.db_connection = None

        self._db_count = 0
        self._dbs = {}
        self._cursor_count = 0
        self._cursors = {}

    def connection(self, space):
        if self.db_connection is not None:
            return self.db_connection
        assert self.driver is not None
        print "DBMode: %s" % self.driver
        connection = SQLConnection(space, self.driver, self.db_file_name)
        assert connection is not None
        self.db_connection = connection
        return connection

    def connect(self, space, db_class, filename):
        handle = self._db_count
        self._dbs[handle] = SQLConnection(space, db_class, filename)

        self._db_count += 1

        return space.wrap_int(handle)

    def execute(self, space, s_frame, argcount):
        # import pdb; pdb.set_trace()
        if not 2 <= argcount <= 3:
            raise PrimitiveFailedError(
                'wrong number of arguments: %s' % argcount)

        args = []
        if argcount == 3:
            args = space.unwrap_array(s_frame.pop())

        arg3_w = s_frame.pop()
        sql = space.unwrap_string(arg3_w)
        arg2_w = s_frame.pop()
        db_handle = space.unwrap_longlong(arg2_w)

        db = self._dbs.get(db_handle, None)
        if db is None:
            raise PrimitiveFailedError('execute [db is None]')

        handle = self._cursor_count
        self._cursors[handle] = db.execute(sql, args)

        self._cursor_count += 1

        return space.wrap_int(handle)

    @jit.elidable
    def cursor(self, cursor_handle):
        cursor = self._cursors.get(cursor_handle, None)
        if cursor is None:
            raise PrimitiveFailedError('cursor not found')
        return cursor

    def close(self, space, db_pointer):
        db = self._dbs.get(db_pointer, None)
        if db is None:
            raise PrimitiveFailedError('close [db is None]')
        return space.wrap_bool(db.close())


dbm = DBManager()


###############################################################################
# Primitive Definitions                                                       #
###############################################################################

@DatabasePlugin.expose_primitive(unwrap_spec=[object, str, bool])
def primitiveSQLConnect(interp, s_frame, w_rcvr, filename, sqpyte):
    if sqpyte:
        return dbm.connect(interp.space, interpreter.SQPyteDB, filename)
    return dbm.connect(interp.space, interpreter.SQLite3DB, filename)


@DatabasePlugin.expose_primitive(clean_stack=False)
def primitiveSQLExecute(interp, s_frame, argcount):
    return dbm.execute(interp.space, s_frame, argcount)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLNext(interp, s_frame, w_rcvr, cursor_handle):
    return dbm.cursor(cursor_handle).next()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLColumnCount(interp, s_frame, w_rcvr, cursor_handle):
    return dbm.cursor(cursor_handle).column_count()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLColumnNames(interp, s_frame, w_rcvr, cursor_handle):
    return dbm.cursor(cursor_handle).column_names()


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int, int])
def primitiveSQLColumnName(interp, s_frame, w_rcvr, cursor_handle, index):
    if index < 1:
        raise PrimitiveFailedError('Index must be >= 1')

    # Smalltalk counts from 1, rest of world from 0
    return dbm.cursor(cursor_handle).column_name(index - 1)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLClose(interp, s_frame, w_rcvr, db_handle):
    return dbm.close(interp.space, db_handle)


@DatabasePlugin.expose_primitive(unwrap_spec=[object, int])
def primitiveSQLModeSwitch(interp, s_frame, w_rcvr, mode):
    if mode == 1:
        dbm.driver = interpreter.SQLite3DB
    elif mode == 2:
        dbm.driver = interpreter.SQPyteDB
    else:
        dbm.driver = None
    return interp.space.w_nil

@DatabasePlugin.expose_primitive(unwrap_spec=[object, str])
def primitiveSetDBFile(interp, s_frame, w_rcvr, db_file_name):
    dbm.db_file_name = db_file_name
    return interp.space.w_nil

@DatabasePlugin.expose_primitive(unwrap_spec=[object])
def primitiveCloseDBObject(interp, s_frame, w_rcvr):
    dbm.connection(interp.space).close()
    return interp.space.w_nil
