from rsqueakvm import constants, error
from rsqueakvm.model.pointers import W_PointersObject
from rpython.rlib import objectmodel, jit
from rsqueakvm.plugins.database import SQLConnection
from sqpyte import interpreter
from rsqueakvm.error import PrimitiveFailedError
import pdb


class W_DBObject(W_PointersObject):

    db_connection = None
    id_counter = 0

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        super(W_DBObject, self).__init__(space, w_class, size, weak)
        self.column_names = {}
        self.id = W_DBObject.id_counter
        self.w_id = space.wrap_int(self.id)
        W_DBObject.id_counter += 1

        if not W_DBObject.db_connection:
            print("Establish connection")
            W_DBObject.db_connection = SQLConnection(space, interpreter.SQLite3DB, ":memory:")

        # remove " class" from the classname
        self.class_name = w_class.classname(space).split(" ")[0]

        print "CREATE TABLE IF NOT EXISTS", self.class_name, "(id integer)"
        create_sql = "CREATE TABLE IF NOT EXISTS " + self.class_name + " (id INTEGER);"
        W_DBObject.db_connection.execute(create_sql)
        print "insert into " + self.class_name + " values ('" + str(self.id) + "');"
        W_DBObject.db_connection.execute("insert into " + self.class_name + " values (" + str(self.id) + ");")


    def fetch(self, space, n0):
        print("Fetch in", self.class_name, n0)
        query_sql = "SELECT '%s' FROM %s WHERE id=?" % (n0, self.class_name)
        cursor = W_DBObject.db_connection.execute(query_sql, [self.w_id])
        w_result = space.unwrap_array(cursor.next())
        pdb.set_trace()
        if w_result:
            return w_result[0]
        else:
            raise PrimitiveFailedError

    def store(self, space, n0, w_value):

        aType = "blob"
        cls = w_value.getclass(space)
        if (cls.is_same_object(space.w_String)):
            aType = "text"
        elif cls.is_same_object(space.w_SmallInteger):
            aType = "integer"
        elif cls.is_same_object(space.w_Float):
            aType = "real"
        elif cls.is_same_object(space.w_nil):
            return
        else:
            raise PrimitiveFailedError(
                'unable to unwrap %s' % w_value.getclass(space))

        if not n0 in self.column_names:
            print("alter table", self.class_name, "add column", '"' + str(n0) + '"', aType)

            alter_sql = "alter table " + self.class_name + " add column " + '"' + str(n0) + '" ' + aType
            W_DBObject.db_connection.execute(alter_sql)

            self.column_names[n0] = True

        print("Store in", self.class_name, n0, w_value)

        # update_sql = "update %s set '%s'=? where id=?" % (self.class_name, n0)
        update_sql = "update %s set '0'=123 where id=0" % (self.class_name)
        # W_DBObject.db_connection.execute(update_sql, [w_value, self.w_id])
        W_DBObject.db_connection.execute(update_sql)

        return self._get_strategy().store(self, n0, w_value)

