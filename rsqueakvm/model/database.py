from rsqueakvm.model.pointers import W_PointersObject
from rpython.rlib import jit
from rsqueakvm.plugins.database import dbm, SQLConnection
from rsqueakvm.error import PrimitiveFailedError


NIL = ''
TEXT = 'text'
INTEGER = 'integer'
REAL = 'real'
BLOB = 'blob'


class W_DBObject_State:
    _immutable_fields_ = ["db_connection?", "column_types_for_table",
                          "db_objects", "class_names"]

    def __init__(self):
        self.db_connection = None
        self.id_counter = 0
        self.column_types_for_table = {}
        # Maps from DBObject id to DBObject and only includes DBObjects which
        # are referenced from an attribute of a DBObject.
        self.db_objects = {}
        self.class_names = {}

    def get_column_type(self, class_name, n0):
        str_type = self.get_column_types(class_name)
        if str_type != NIL:
            return jit.promote(str_type)
        else:
            return NIL

    @jit.elidable
    def get_column_types(self, class_name):
        return self.column_types_for_table[class_name]

    def set_column_type(self, class_name, position, value):
        self.get_column_types(class_name)[position] = value

    # This is only ever called once per classname. We always promote the
    # classname to a constant, so any time the classname changes, we have to
    # break out of the trace and compile a new bridge, anyway. When that
    # happens, this was already run once, so we don't need to do it again.
    @jit.not_in_trace
    def create_column_types_if_neccessary(self, class_name, size):
        if class_name not in self.column_types_for_table:
            W_DBObject.state.column_types_for_table[class_name] = [''] * size

    # Same reason as above
    @jit.not_in_trace
    def create_table_if_neccessary(self, class_name, connection):
        if class_name not in W_DBObject.state.class_names:
            create_sql = ("CREATE TABLE IF NOT EXISTS %s (id INTEGER);" %
                          class_name)
            connection.execute(create_sql)
            W_DBObject.state.class_names[class_name] = True


class W_DBObject(W_PointersObject):
    _attrs_ = ["id"]
    _immutable_fields_ = ["id"]
    state = W_DBObject_State()

    @staticmethod
    def connection(space):
        if W_DBObject.state.db_connection is not None:
            return W_DBObject.state.db_connection
        assert dbm.driver is not None
        print "DBMode: %s" % dbm.driver
        connection = SQLConnection(space, dbm.driver, ":memory:")
        assert connection is not None
        W_DBObject.state.db_connection = connection
        return connection

    @staticmethod
    def next_id():
        theId = W_DBObject.state.id_counter
        W_DBObject.state.id_counter += 1
        return theId

    @staticmethod
    @jit.elidable
    def _insert_sql(class_name):
        return "INSERT INTO %s (id) VALUES (?);" % class_name

    @staticmethod
    @jit.elidable
    def _select_sql(class_name, n0):
        return ("SELECT inst_var_%s FROM %s WHERE id=?;" %
                (n0, class_name))

    @staticmethod
    @jit.elidable
    def _alter_sql(class_name, n0, aType):
        return ("ALTER TABLE %s ADD COLUMN inst_var_%s %s;" %
                (class_name, n0, aType))

    @staticmethod
    @jit.elidable
    def _update_sql(class_name, n0):
        return "UPDATE %s SET inst_var_%s=? WHERE id=?" % (class_name, n0)

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        W_PointersObject.__init__(self, space, w_class, size, weak)
        self.id = W_DBObject.next_id()

        class_name = self.class_name(space)
        W_DBObject.state.create_column_types_if_neccessary(class_name, size)
        connection = W_DBObject.connection(space)
        W_DBObject.state.create_table_if_neccessary(class_name, connection)
        connection.execute(W_DBObject._insert_sql(class_name),
                           [self.w_id(space)])

    def class_name(self, space):
        return jit.promote(self.classname(space))

    def w_id(self, space):
        return space.wrap_int(self.id)

    def fetch(self, space, n0):
        class_name = self.class_name(space)
        if not W_DBObject.state.get_column_type(class_name, n0):
            # print "Can't find column. Falling back to default fetch."
            return W_PointersObject.fetch(self, space, n0)

        connection = W_DBObject.connection(space)
        cursor = connection.execute(W_DBObject._select_sql(class_name, n0),
                                    [self.w_id(space)])

        w_result = space.unwrap_array(cursor.next())
        if w_result:
            if W_DBObject.state.get_column_type(class_name, n0) is BLOB:
                db_id = space.unwrap_int(w_result[0])
                return W_DBObject.state.db_objects[db_id]
            else:
                return w_result[0]
        else:
            raise PrimitiveFailedError

    def store(self, space, n0, w_value):
        cls = w_value.getclass(space)
        if (cls.is_same_object(space.w_String)):
            aType = TEXT
        elif cls.is_same_object(space.w_SmallInteger):
            aType = INTEGER
        elif cls.is_same_object(space.w_Float):
            aType = REAL
        elif cls.is_same_object(space.w_nil):
            aType = NIL
        else:
            if isinstance(w_value, W_DBObject):
                aType = BLOB
                W_DBObject.state.db_objects[w_value.id] = w_value
                # Save id in database.
                w_value = w_value.w_id(space)
            else:
                # print 'Unable to unwrap %s' % w_value.getclass(space)
                # print 'Falling back to standard store.'
                return W_PointersObject.store(self, space, n0, w_value)

        aType = jit.promote_string(aType)
        class_name = self.class_name(space)

        if (aType is not NIL and
                W_DBObject.state.get_column_type(class_name, n0) is NIL):
            W_DBObject.connection(space).execute(
                W_DBObject._alter_sql(class_name, n0, aType))
            # print "invalidate cache"
            W_DBObject.connection(space).statement_cache.invalidate()
            W_DBObject.state.set_column_type(class_name, n0, aType)

        connection = W_DBObject.connection(space)
        connection.execute(W_DBObject._update_sql(class_name, n0),
                           [w_value, self.w_id(space)])
