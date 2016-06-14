from rsqueakvm import constants, error
from rsqueakvm.model.pointers import W_PointersObject
from rpython.rlib import objectmodel, jit
import pdb


class W_DBObject(W_PointersObject):

    db_connection = None
    id_counter = 0
    db_pointers = {}

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        super(W_DBObject, self).__init__(space, w_class, size, weak)
        self.column_names = {}
        self.id = W_DBObject.id_counter
        W_DBObject.id_counter += 1

        if not W_DBObject.db_connection:
            print("Establish connection")
            W_DBObject.db_connection = "New Connection"

        self.class_name = w_class.classname(space)
        if not self.class_name in W_DBObject.db_pointers:
            print("CREATE TABLE", self.class_name, "(id integer)")
            W_DBObject.db_pointers[self.class_name] = True


    def fetch(self, space, n0):
        print("Fetch in", self.class_name, n0)
        return self._get_strategy().fetch(self, n0)

    def store(self, space, n0, w_value):

        if not n0 in self.column_names:
            aType = "blob"
            w_value_type = type(w_value)
            # TODO: type checking does not work this way, since primitive types
            # are wrapped
            if w_value_type == int:
                aType = "integer"
            elif w_value_type == str:
                aType = "text"
            elif w_value_type == float:
                aType = "real"
            print("alter table", self.class_name, "add column", '"' + str(n0) + '"', aType)
            self.column_names[n0] = True

        print("Store in", self.class_name, n0, w_value)
        return self._get_strategy().store(self, n0, w_value)