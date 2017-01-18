from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.numeric import W_Float, W_SmallInteger
from rsqueakvm.plugins.python.model import W_PythonObject
from rsqueakvm.plugins.python.global_state import py_space
from rsqueakvm.model.variable import W_BytesObject

from pypy.objspace.std.boolobject import W_BoolObject as WP_BoolObject
from pypy.objspace.std.bytesobject import W_BytesObject as WP_BytesObject
from pypy.objspace.std.floatobject import W_FloatObject as WP_FloatObject
from pypy.objspace.std.intobject import W_IntObject as WP_IntObject
from pypy.objspace.std.listobject import W_ListObject as WP_ListObject
from pypy.objspace.std.noneobject import W_NoneObject as WP_NoneObject
from pypy.objspace.std.tupleobject import W_TupleObject as WP_TupleObject

from rpython.rlib import objectmodel


@objectmodel.specialize.argtype(0)
def wrap(space, wp_object):
    # import pdb; pdb.set_trace()
    if isinstance(wp_object, WP_FloatObject):
        return space.wrap_float(py_space.float_w(wp_object))
    elif isinstance(wp_object, WP_BytesObject):
        return space.wrap_string(py_space.str_w(wp_object))
    elif isinstance(wp_object, WP_ListObject):
        return space.wrap_list(
            [wrap(space, item) for item in wp_object.getitems()])
    elif isinstance(wp_object, WP_TupleObject):
        return space.wrap_list(
            [wrap(space, item) for item in wp_object.tolist()])
    elif wp_object is None or isinstance(wp_object, WP_NoneObject):
        return space.w_nil
    elif isinstance(wp_object, WP_IntObject):
        # WP_BoolObject inherits from WP_IntObject
        if wp_object is WP_BoolObject.w_False:
            return space.w_false
        elif wp_object is WP_BoolObject.w_True:
            return space.w_true
        return space.wrap_int(py_space.int_w(wp_object))
    else:
        return W_PythonObject(wp_object)


@objectmodel.specialize.argtype(0)
def unwrap(space, w_object):
    if isinstance(w_object, W_PythonObject):
        return w_object.wp_object
    elif isinstance(w_object, W_Float):
        return py_space.newfloat(space.unwrap_float(w_object))
    elif isinstance(w_object, W_SmallInteger):
        return py_space.newint(space.unwrap_int(w_object))
    elif isinstance(w_object, W_BytesObject):
        # if w_object.getclass(space).is_same_object(space.w_String):
        return py_space.newbytes(space.unwrap_string(w_object))
    # import pdb; pdb.set_trace()
    print 'Cannot unwrap %s' % w_object
    raise PrimitiveFailedError


def call_method(space, wp_rcvr, methodname, args_w):
    args_w_len = len(args_w)
    if args_w_len == 1:
        arg1 = unwrap(space, args_w[0])
        return wrap(space, py_space.call_method(wp_rcvr, methodname, arg1))
    elif args_w_len == 2:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        return wrap(space, py_space.call_method(wp_rcvr, methodname,
                                                arg1, arg2))
    elif args_w_len == 3:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        return wrap(space, py_space.call_method(wp_rcvr, methodname,
                                                arg1, arg2, arg3))
    elif args_w_len == 4:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        return wrap(space, py_space.call_method(wp_rcvr, methodname,
                                                arg1, arg2, arg3, arg4))
    return wrap(space, py_space.call_method(wp_rcvr, methodname))


def call_function(space, wp_func, args_w):
    args_w_len = len(args_w)
    if args_w_len == 1:
        arg1 = unwrap(space, args_w[0])
        return wrap(space, py_space.call_function(wp_func, arg1))
    elif args_w_len == 2:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        return wrap(space, py_space.call_function(wp_func, arg1, arg2))
    elif args_w_len == 3:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        return wrap(space, py_space.call_function(wp_func, arg1, arg2, arg3))
    elif args_w_len == 4:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        return wrap(space, py_space.call_function(wp_func,
                                                  arg1, arg2, arg3, arg4))
    return wrap(space, py_space.call_function(wp_func))
