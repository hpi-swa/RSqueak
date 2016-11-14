import py
import sys

from rsqueakvm import objspace, error, constants
# from rsqueakvm.model.variable import W_BytesObject

from rpython.rlib.rarithmetic import r_uint, r_longlong

from .util import create_space, copy_to_module, cleanup_module


def setup_module():
    space = create_space(bootstrap = True)
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def ismetaclass(w_cls):
    # Heuristic to detect if this is a metaclass. Don't use apart
    # from in this test file, because classtable['w_Metaclass'] is
    # bogus after loading an image.
    return w_cls.getclass(None) is space.classtable['w_Metaclass']

def test_every_class_is_an_instance_of_a_metaclass():
    for (nm, w_cls) in space.classtable.items():
        assert ismetaclass(w_cls) or ismetaclass(w_cls.getclass(None))

def test_every_metaclass_inherits_from_class_and_behavior():
    s_Class = space.classtable['w_Class'].as_class_get_shadow(space)
    s_Behavior = space.classtable['w_Behavior'].as_class_get_shadow(space)
    for (nm, w_cls) in space.classtable.items():
        if ismetaclass(w_cls):
            shadow = w_cls.as_class_get_shadow(space)
            assert shadow.inherits_from(s_Class)
    assert s_Class.inherits_from(s_Behavior)

def test_metaclass_of_metaclass_is_an_instance_of_metaclass():
    w_Metaclass = space.classtable['w_Metaclass']
    assert w_Metaclass.getclass(space).getclass(space) is w_Metaclass

def test_ruint():
    """
    | a b |
    a := (9223372036854775808).
    b := LargePositiveInteger new: a size + 1.
    1 to: a size do: [:index |
        b digitAt: index put: (a digitAt: index)].
    b digitAt: (a size + 1) put: 1.
    b.
    => 27670116110564327424
    """

    for num in [0, 1, 41, 100, 2**31, sys.maxint + 1, -1]:
        num = r_uint(num)
        assert space.unwrap_uint(space.wrap_int(num)) == num


def test_wrap_int():
    for num in [-10, 1, 15, 0x3fffffff]:
        assert space.wrap_int(num).value == num

    sbit = (constants.LONG_BIT-1)
    for num in [r_longlong(2**sbit - 1), r_longlong(-(2**sbit))]:
        assert space.wrap_int(num).unwrap_long_untranslated(space) == num
    for num in [-(2**sbit + 1)]:
        with py.test.raises(error.WrappingError):
            space.wrap_int(num)
