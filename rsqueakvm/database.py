from rsqueakvm.error import PrimitiveFailedError

from rpython.rlib import jit
from rpython.rtyper.lltypesystem import rffi

try:
    from sqpyte.capi import CConfig
except ImportError:
    class CConfig():
        SQLITE_TEXT = SQLITE_BLOB = SQLITE_INTEGER = SQLITE_FLOAT = None
        SQLITE_NULL = SQLITE_ROW = SQLITE_DONE = None


###############################################################################
# SQL Connection And Cursor Implementation                                    #
###############################################################################

class SQLConnection(object):
    _immutable_fields_ = ['db', 'statement_cache']

    def __init__(self, db_class, filename):
        self.statement_cache = StatementCache(self)
        self.is_closed = False

        self.connect(db_class, filename)

    def connect(self, db_class, filename):
        try:
            print 'Trying to connect to %s...' % filename
            self.db = db_class(filename)
            print 'Success'
        except Exception as e:
            print 'Unable to connect to database: ', e

    def cursor(self):
        return SQLCursor(self)

    def execute(self, space, sql, args=None):
        return self.cursor().execute(space, sql, args)

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
    _immutable_fields_ = ['connection']

    def __init__(self, connection):
        assert isinstance(connection, SQLConnection)
        self.connection = connection
        self.statement = None

    @jit.unroll_safe
    def execute(self, space, sql, args=None):
        jit.promote(self.connection)
        jit.promote(self.statement)
        cache = self.connection.statement_cache
        self.statement = cache.get_or_make(sql)

        if args is not None:
            query = self.statement.query
            if len(args) != query.bind_parameter_count():
                raise PrimitiveFailedError('wrong # of arguments for query')
            for i, w_value in enumerate(args):
                self.bind_query_argument(space, w_value, query, i + 1)

        self._step()
        return self

    def bind_query_argument(self, space, w_value, query, i):
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

    def next(self, space):
        if jit.promote(self.statement) is None:
            return space.w_nil
        row = self._fetch_one_row(space)
        self._step()
        # This should be unroll safe, since _fetch_one_row() was also marked
        # unroll_safe.
        return space.wrap_list_unroll_safe(row)

    def raw_next(self):
        if jit.promote(self.statement) is None:
            return None
        query = self.statement.query
        self._step()
        return query

    def column_count(self):
        return self.statement.query.data_count()

    def column_name(self, index):
        query = self.statement.query
        assert query is not None
        return rffi.charp2strn(query.column_name(index), 255)

    def column_names(self):
        names = []
        for i in range(0, self.column_count()):
            names.append(self.column_name(i))
        return names

    def close(self):
        if self.statement:
            self.statement.close()
            self.statement = None

    @jit.unroll_safe
    def _fetch_one_row(self, space):
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
                w_result = space.wrap_string(result)  # no encoding
            elif tid == CConfig.SQLITE_INTEGER:
                result = query.column_int64(i)
                w_result = space.wrap_int(result)
            elif tid == CConfig.SQLITE_FLOAT:
                result = query.column_double(i)
                w_result = space.wrap_float(result)
            elif tid == CConfig.SQLITE_NULL:
                w_result = space.w_nil
            else:
                raise PrimitiveFailedError('read_row [tid: %s' % tid)
            cols[i] = w_result
        return cols

    def _step(self):
        rc = self.statement.query.mainloop()
        if rc == CConfig.SQLITE_ROW:
            pass
        elif rc == CConfig.SQLITE_DONE:
            self._reset()
        else:
            raise PrimitiveFailedError('strange result: %s' % rc)

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
        except Exception as e:
            raise PrimitiveFailedError(str(e))
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
        self.db_file_name = ":memory:"
        self.db_connection = None
        self.driver = None  # Driver for DBObjects
        try:
            from sqpyte.interpreter import SQPyteDB
            self.driver = SQPyteDB
        except ImportError:
            pass

        self._db_count = 0
        self._dbs = {}
        self._cursor_count = 0
        self._cursors = {}

    def connection(self):
        if self.db_connection is not None:
            return self.db_connection
        assert self.driver is not None
        print "DBMode: %s" % self.driver
        connection = SQLConnection(self.driver, self.db_file_name)
        assert connection is not None
        self.db_connection = connection
        return connection

    def connect(self, db_class, filename):
        handle = self._db_count
        self._dbs[handle] = SQLConnection(db_class, filename)
        self._db_count += 1
        return handle

    def get_connection(self, db_handle):
        db = self._dbs.get(db_handle, None)
        if db is None:
            raise PrimitiveFailedError('execute [db is None]')
        return db

    def execute(self, space, db, sql, args=None):
        handle = self._cursor_count
        self._cursors[handle] = db.execute(space, sql, args)
        self._cursor_count += 1
        return handle

    @jit.elidable
    def cursor(self, cursor_handle):
        cursor = self._cursors.get(cursor_handle, None)
        if cursor is None:
            raise PrimitiveFailedError('cursor not found')
        return cursor

    def close(self, db_pointer):
        db = self._dbs.get(db_pointer, None)
        if db is None:
            raise PrimitiveFailedError('close [db is None]')
        return db.close()

dbm = DBManager()
