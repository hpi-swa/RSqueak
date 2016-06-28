from rsqueakvm.model.pointers import W_PointersObject
from rpython.rlib import jit
from rsqueakvm.plugins.database import dbm, SQLConnection
from rsqueakvm.error import PrimitiveFailedError


class W_DBObject(W_PointersObject):

    _attrs_ = ["id", "class_name"]
    db_connection = [None]
    id_counter = [0]
    column_types_for_table = {}
    # Maps from DBObject id to DBObject and only includes DBObjects which are
    # referenced from an attribute of a DBObject.
    db_objects = {}

    @staticmethod
    def connection(space):
        if W_DBObject.db_connection[0] is None:
            # print("Establish connection")
            assert dbm.driver is not None
            print "DBMode: %s" % dbm.driver
            connection = SQLConnection(space, dbm.driver, ":memory:")
            W_DBObject.db_connection[0] = connection
        assert W_DBObject.db_connection[0] is not None
        return W_DBObject.db_connection[0]

    @staticmethod
    def next_id():
        theId = W_DBObject.id_counter[0]
        W_DBObject.id_counter[0] += 1
        return theId

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        W_PointersObject.__init__(self, space, w_class, size, weak)
        self.id = W_DBObject.next_id()

        # remove " class" from the classname
        self.class_name = w_class.classname(space).split(" ")[0]
        if self.class_name not in W_DBObject.column_types_for_table:
            W_DBObject.column_types_for_table[self.class_name] = {}

        create_sql = ("CREATE TABLE IF NOT EXISTS %s (id INTEGER);" %
                      self.class_name)
        # print create_sql

        W_DBObject.connection(space).execute(create_sql)
        insert_sql = "INSERT INTO %s (id) VALUES (?);" % self.class_name
        # print insert_sql
        W_DBObject.connection(space).execute(insert_sql, [self.w_id(space)])

    def w_id(self, space):
        return space.wrap_int(self.id)

    def get_column_types(self):
        return W_DBObject.column_types_for_table[self.class_name]

    def fetch(self, space, n0):
        if n0 not in self.get_column_types():
            # print "Can't find column. Falling back to default fetch."
            return W_PointersObject.fetch(self, space, n0)

        query_sql = ("SELECT inst_var_%s FROM %s WHERE id=?;" %
                     (n0, self.class_name))
        # print query_sql
        connection = W_DBObject.connection(space)
        cursor = connection.execute(query_sql, [self.w_id(space)])

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
                return W_PointersObject.store(self, space, n0, w_value)

        if aType != "__nil__" and n0 not in self.get_column_types():
            alter_sql = ("ALTER TABLE %s ADD COLUMN inst_var_%s %s;" %
                         (self.class_name, n0, aType))
            # print alter_sql
            W_DBObject.connection(space).execute(alter_sql)
            # print "invalidate cache"
            W_DBObject.connection(space).statement_cache.invalidate()

            self.get_column_types()[n0] = aType

        update_sql = ("UPDATE %s SET inst_var_%s=? WHERE id=?" %
                      (self.class_name, n0))
        # print update_sql
        connection = W_DBObject.connection(space)
        connection.execute(update_sql, [w_value, self.w_id(space)])
