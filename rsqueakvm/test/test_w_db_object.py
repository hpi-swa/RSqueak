from rsqueakvm import interpreter, storage_classes, storage_contexts, wrapper, constants, error
from rsqueakvm.test.test_interpreter import bootstrap_class
from rsqueakvm.model.database import W_DBObject
from rsqueakvm.model.pointers import W_PointersObject

from .util import create_space

def bootstrap_class(instsize, name, w_superclass=None, w_metaclass=None,
                    format=storage_classes.POINTERS, varsized=True):
    space = create_space(bootstrap = True)
    return space.bootstrap_class(instsize, w_superclass, w_metaclass,
                    name, format, varsized)

def test_fetch_and_store_with_string():
    space = create_space(bootstrap = True)
    instsize = 10
    obj = W_DBObject(space, bootstrap_class(instsize, "Foo"), instsize)
    val = "test-string"
    obj.store(space, 0, space.wrap_string(val))
    returned_val = obj.fetch(space, 0)

    assert val == space.unwrap_string(returned_val)


def test_fetch_and_store_with_pointers_object():
    space = create_space(bootstrap = True)
    instsize = 10
    obj = W_DBObject(space, bootstrap_class(instsize, "Foo"), instsize)
    val = W_PointersObject(space, bootstrap_class(instsize, "Foo"), instsize)
    obj.store(space, 0, val)
    returned_val = obj.fetch(space, 0)

    assert val == returned_val


def test_fetch_and_store_with_w_db_object():
    space = create_space(bootstrap = True)
    instsize = 10
    obj = W_DBObject(space, bootstrap_class(instsize, "Doo"), instsize)
    val = W_DBObject(space, bootstrap_class(instsize, "Bar"), instsize)
    obj.store(space, 0, val)
    returned_val = obj.fetch(space, 0)

    assert val == returned_val
