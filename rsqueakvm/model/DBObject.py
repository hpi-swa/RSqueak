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
    column_names_for_table = {}

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        super(W_DBObject, self).__init__(space, w_class, size, weak)
        self.id = W_DBObject.id_counter
        self.w_id = space.wrap_int(self.id)
        W_DBObject.id_counter += 1

        if not W_DBObject.db_connection:
            print("Establish connection")
            W_DBObject.db_connection = SQLConnection(space, interpreter.SQLite3DB, ":memory:")

        # remove " class" from the classname
        self.class_name = w_class.classname(space).split(" ")[0]
        if not self.class_name in W_DBObject.column_names_for_table:
            W_DBObject.column_names_for_table[self.class_name] = {}

        create_sql = "CREATE TABLE IF NOT EXISTS %s (id INTEGER);" % self.class_name
        print create_sql
        W_DBObject.db_connection.execute(create_sql)
        insert_sql = "insert into %s (id) values (?);" % self.class_name
        print insert_sql
        W_DBObject.db_connection.execute(insert_sql, [self.w_id])


    def fetch(self, space, n0):
        print("Fetch in", self.class_name, n0)
        query_sql = "SELECT inst_var_%s FROM %s WHERE id=?;" % (n0, self.class_name)
        cursor = W_DBObject.db_connection.execute(query_sql, [self.w_id])

        w_result = space.unwrap_array(cursor.next())
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

        if not n0 in W_DBObject.column_names_for_table[self.class_name]:
            alter_sql = "alter table %s add column inst_var_%s %s;" % (self.class_name, n0, aType)
            print alter_sql
            W_DBObject.db_connection.execute(alter_sql)

            W_DBObject.column_names_for_table[self.class_name][n0] = True

        update_sql = "update %s set inst_var_%s=? where id=?" % (self.class_name, n0)
        print update_sql
        W_DBObject.db_connection.execute(update_sql, [w_value, self.w_id])

        return self._get_strategy().store(self, n0, w_value)

