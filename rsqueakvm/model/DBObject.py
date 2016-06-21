from rsqueakvm import constants, error
from rsqueakvm.model.pointers import W_PointersObject
from rpython.rlib import objectmodel, jit
from rsqueakvm.plugins.database import SQLConnection
from sqpyte import interpreter
from rsqueakvm.error import PrimitiveFailedError


class W_DBObject(W_PointersObject):

    _attrs_ = ["id", "class_name"]
    db_connection = None
    id_counter = 0
    column_types_for_table = {}
    # Maps from DBObject id to DBObject and only includes DBObjects which are
    # referenced from an attribute of a DBObject.
    db_objects = {}

    @staticmethod
    def connection(space):
        if W_DBObject.db_connection is None:
            # print("Establish connection")
            W_DBObject.db_connection = SQLConnection(space, interpreter.SQLite3DB, ":memory:")
        assert W_DBObject.db_connection is not None
        return W_DBObject.db_connection

    @staticmethod
    def next_id():
        theId = W_DBObject.id_counter
        W_DBObject.id_counter += 1
        return theId

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        W_DBObject.__init__(self, space, w_class, size, weak)
        self.id = W_DBObject.next_id()


        # remove " class" from the classname
        self.class_name = w_class.classname(space).split(" ")[0]
        if not self.class_name in W_DBObject.column_types_for_table:
            W_DBObject.column_types_for_table[self.class_name] = {}

        create_sql = "CREATE TABLE IF NOT EXISTS %s (id INTEGER);" % self.class_name
        # print create_sql
        W_DBObject.connection().execute(create_sql)
        insert_sql = "insert into %s (id) values (?);" % self.class_name
        # print insert_sql
        W_DBObject.connection().execute(insert_sql, [self.w_id(space)])

    def w_id(self, space):
        return space.wrap_int(self.id)

    def get_column_types(self):
        return W_DBObject.column_types_for_table[self.class_name]

    def fetch(self, space, n0):
        if n0 not in self.get_column_types():
            # print "Can't find column. Falling back to default fetch."
            return W_DBObject.fetch(self, space, n0)

        query_sql = "SELECT inst_var_%s FROM %s WHERE id=?;" % (n0, self.class_name)
        # print query_sql
        cursor = W_DBObject.connection().execute(query_sql, [self.w_id(space)])

        w_result = space.unwrap_array(cursor.next())
        if w_result:
            if self.get_column_types()[n0] == "blob":
                db_id = space.unwrap_int(w_result[0])
                return W_DBObject.db_objects[db_id]
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
                W_DBObject.db_objects[w_value.id] = w_value
                # Save id in database.
                w_value = w_value.w_id(space)
            else:
                # print 'Unable to unwrap %s' % w_value.getclass(space)
                # print 'Falling back to standard store.'
                return W_DBObject.store(self, space, n0, w_value)

        if aType != "__nil__" and not n0 in self.get_column_types():
            alter_sql = "alter table %s add column inst_var_%s %s;" % (self.class_name, n0, aType)
            # print alter_sql
            W_DBObject.connection().execute(alter_sql)

            self.get_column_types()[n0] = aType

        update_sql = "update %s set inst_var_%s=? where id=?" % (self.class_name, n0)
        # print update_sql
        W_DBObject.connection().execute(update_sql, [w_value, self.w_id(space)])

        return self._get_strategy().store(self, n0, w_value)
