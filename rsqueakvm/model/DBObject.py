from rsqueakvm import constants, error
from rsqueakvm.model.pointers import W_PointersObject
from rpython.rlib import objectmodel, jit
import pdb


class W_DBObject(W_PointersObject):

    # map from className to db pointer
    aMap = {"TestDBObject": 0}

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        super(W_DBObject, self).__init__(space, w_class, size, weak)
        pdb.set_trace()
        # does db pointer exist?
            # create table


    def fetch(self, space, n0):
        pdb.set_trace()
        return self._get_strategy().fetch(self, n0)

    def store(self, space, n0, w_value):
        pdb.set_trace()
        return self._get_strategy().store(self, n0, w_value)