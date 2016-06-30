from rsqueakvm.model.pointers import W_PointersObject
from rpython.rlib import jit
from rsqueakvm.plugins.database import dbm, SQLConnection
from rsqueakvm.error import PrimitiveFailedError


class W_DBObject_State:
    def __init__(self):
        self.id_counter = 0
        self.column_types_for_table = {}
        # Maps from DBObject id to DBObject and only includes DBObjects which
        # are referenced from an attribute of a DBObject.
        self.db_objects = {}
        self.class_names = {}

    @jit.elidable
    def get_column_types(self, w_dbobject):
        return self.column_types_for_table[w_dbobject.class_name]

    def set_column_type(self, w_dbobject, position, value):
        self.get_column_types(w_dbobject)[position] = value


class W_DBObject(W_PointersObject):

    _attrs_ = ["id", "class_name"]
    state = W_DBObject_State()

    @staticmethod
    def next_id():
        theId = W_DBObject.state.id_counter
        W_DBObject.state.id_counter += 1
        return theId

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        W_PointersObject.__init__(self, space, w_class, size, weak)
        self.id = W_DBObject.next_id()

        # remove " class" from the classname
        self.class_name = w_class.classname(space).split(" ")[0]
        if self.class_name not in W_DBObject.state.column_types_for_table:
            W_DBObject.state.column_types_for_table[self.class_name] = [''] * size

        connection = dbm.connection(space)
        if self.class_name not in W_DBObject.state.class_names:
            create_sql = ("CREATE TABLE IF NOT EXISTS %s (id INTEGER);" %
                          self.class_name)
            connection.execute(create_sql)
            W_DBObject.state.class_names[self.class_name] = True

        connection.execute(self._insert_sql(), [self.w_id(space)])

    def w_id(self, space):
        return space.wrap_int(self.id)

    @jit.elidable
    def _insert_sql(self):
        return "INSERT INTO %s (id) VALUES (?);" % self.class_name

    @jit.elidable
    def _select_sql(self, n0):
        return ("SELECT inst_var_%s FROM %s WHERE id=?;" %
                (n0, self.class_name))

    @jit.elidable
    def _alter_sql(self, n0, aType):
        return ("ALTER TABLE %s ADD COLUMN inst_var_%s %s;" %
                (self.class_name, n0, aType))

    @jit.elidable
    def _update_sql(self, n0):
        return "UPDATE %s SET inst_var_%s=? WHERE id=?" % (self.class_name, n0)

    def fetch(self, space, n0):
        if not W_DBObject.state.get_column_types(self)[n0]:
            # print "Can't find column. Falling back to default fetch."
            return W_PointersObject.fetch(self, space, n0)

        connection = dbm.connection(space)
        cursor = connection.execute(self._select_sql(n0), [self.w_id(space)])

        w_result = space.unwrap_array(cursor.next())
        if w_result:
            if W_DBObject.state.get_column_types(self)[n0] == "blob":
                db_id = space.unwrap_int(w_result[0])
                return W_DBObject.state.db_objects[db_id]
            else:
                return w_result[0]
        else:
            raise PrimitiveFailedError

    def store(self, space, n0, w_value):
        cls = w_value.getclass(space)
        if (cls.is_same_object(space.w_String)):
            aType = "text"
        elif cls.is_same_object(space.w_SmallInteger):
            aType = "integer"
        elif cls.is_same_object(space.w_Float):
            aType = "real"
        elif cls.is_same_object(space.w_nil):
            aType = "__nil__"
        else:
            if isinstance(w_value, W_DBObject):
                aType = "blob"
                W_DBObject.state.db_objects[w_value.id] = w_value
                # Save id in database.
                w_value = w_value.w_id(space)
            else:
                # print 'Unable to unwrap %s' % w_value.getclass(space)
                # print 'Falling back to standard store.'
                return W_PointersObject.store(self, space, n0, w_value)

        if (aType != "__nil__" and
                W_DBObject.state.get_column_types(self)[n0] == ''):
            dbm.connection(space).execute(self._alter_sql(n0, aType))
            # print "invalidate cache"
            dbm.connection(space).statement_cache.invalidate()
            W_DBObject.state.set_column_type(self, n0, aType)

        connection = dbm.connection(space)
        connection.execute(self._update_sql(n0), [w_value, self.w_id(space)])
