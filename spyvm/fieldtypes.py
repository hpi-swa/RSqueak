from spyvm import model, shadow

from rpython.rlib import rerased
from rpython.rlib import objectmodel, jit, signature

class AbstractStorageStrategy():
    _immutable_fields_ = []
    _attrs_ = []
    _settled_ = True

    def __init__(self):
        pass
    def fetch(self, w_obj, n0):
        raise NotImplementedError("Abstract base class")
    def store(self, w_obj, n0, w_val):
        raise NotImplementedError("Abstract base class")
    def size_of(self, w_obj):
        raise NotImplementedError("Abstract base class")
    def initial_storage(self, size, default_element):
        raise NotImplementedError("Abstract base class")
    def storage_for_list(self, collection):
        raise NotImplementedError("Abstract base class")
    def all_vars(self, w_obj):
        return [self.fetch(w_obj, i) for i in range(0, self.size_of(w_obj))]

# This is the regular storage strategy that does not result in any
# optimizations but can handle every case. Applicable for both
# fixed-sized and var-sized objects.
class ListStorageStrategy(AbstractStorageStrategy):
    erase, unerase = rerased.new_erasing_pair("list-storage-strategy")
    erase = staticmethod(erase)
    unerase = staticmethod(unerase)
    
    def fetch(self, w_obj, n0):
        return self.unerase(w_obj.storage)[n0]
    def store(self, w_obj, n0, w_val):
        self.unerase(w_obj.storage)[n0] = w_val
    def size_of(self, w_obj):
        return len(self.unerase(w_obj.storage))
    def initial_storage(self, size, default_element):
        return self.erase([default_element] * size)
    def storage_for_list(self, collection):
        return self.erase([x for x in collection])
ListStorageStrategy.singleton = ListStorageStrategy()


def strategy_of_size(s_class, size):
    if s_class is None or s_class.isvariable():
        return ListStorageStrategy.singleton
    else:
        # TODO -- add AllNilStorageStrategy
        return ListStorageStrategy.singleton

def strategy_for_list(w_obj, vars):
    if if s_class is None:
        return ListStorageStrategy.singleton
    try:
        is_variable = w_obj.s_class.isvariable()
    except AttributeError:
        return ListStorageStrategy.singleton
    if is_variable:
        # TODO - check the contents of vars and choose a good strategy.
        return ListStorageStrategy.singleton
    return ListStorageStrategy.singleton
    