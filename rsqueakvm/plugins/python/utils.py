import os

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.numeric import W_Float, W_SmallInteger
from rsqueakvm.plugins.python.model import W_PythonObject
from rsqueakvm.plugins.python.objspace import py_space
from rsqueakvm.model.variable import W_BytesObject

from pypy.interpreter.error import OperationError
from pypy.interpreter.main import ensure__main__, compilecode
from pypy.interpreter.module import Module
from pypy.interpreter.pycode import PyCode
from pypy.module.__builtin__ import compiling as py_compiling
from pypy.objspace.std.bytesobject import W_BytesObject as WP_BytesObject
from pypy.objspace.std.floatobject import W_FloatObject as WP_FloatObject
from pypy.objspace.std.intobject import W_IntObject as WP_IntObject
from pypy.objspace.std.listobject import W_ListObject as WP_ListObject
from pypy.objspace.std.tupleobject import W_TupleObject as WP_TupleObject
from pypy.objspace.std.unicodeobject import W_UnicodeObject as WP_UnicodeObject

from rpython.rlib import objectmodel


def _run_eval_string(source, filename, cmd):
    # Adopted from PyPy's main.py
    try:
        ws = py_space.newtext

        pycode = compilecode(py_space, source, filename or '<string>', cmd)

        mainmodule = ensure__main__(py_space)
        if not isinstance(mainmodule, Module):
            print 'mainmodule not an instance of Module.'
            return
        w_globals = mainmodule.w_dict

        py_space.setitem(w_globals, ws('__builtins__'), py_space.builtin)
        if filename is not None:
            py_space.setitem(w_globals, ws('__file__'), ws(filename))

        retval = pycode.exec_code(py_space, w_globals, w_globals)
        if eval:
            return retval
        else:
            return

    except OperationError as operationerr:
        operationerr.record_interpreter_traceback()
        raise


@objectmodel.specialize.argtype(0)
def python_to_smalltalk(space, wp_object):
    # import pdb; pdb.set_trace()
    if isinstance(wp_object, WP_FloatObject):
        return space.wrap_float(py_space.float_w(wp_object))
    elif (isinstance(wp_object, WP_BytesObject) or
            isinstance(wp_object, WP_UnicodeObject)):
        return space.wrap_string(py_space.text_w(wp_object))
    elif isinstance(wp_object, WP_ListObject):
        return space.wrap_list(
            [python_to_smalltalk(space, x) for x in wp_object.getitems()])
    elif isinstance(wp_object, WP_TupleObject):
        return space.wrap_list(
            [python_to_smalltalk(space, x) for x in wp_object.tolist()])
    elif wp_object is None or wp_object is py_space.w_None:
        return space.w_nil
    elif isinstance(wp_object, WP_IntObject):
        # WP_BoolObject inherits from WP_IntObject
        if wp_object is py_space.w_False:
            return space.w_false
        elif wp_object is py_space.w_True:
            return space.w_true
        return space.wrap_int(py_space.int_w(wp_object))
    print 'Cannot convert %s to Smalltalk' % wp_object
    raise PrimitiveFailedError


@objectmodel.specialize.argtype(0)
def smalltalk_to_python(space, w_object):
    if isinstance(w_object, W_PythonObject):
        return w_object.wp_object
    elif w_object is None or w_object is space.w_nil:
        return py_space.w_None
    elif w_object is space.w_true:
        return py_space.w_True
    elif w_object is space.w_false:
        return py_space.w_False
    elif isinstance(w_object, W_Float):
        return py_space.newfloat(space.unwrap_float(w_object))
    elif isinstance(w_object, W_SmallInteger):
        return py_space.newint(space.unwrap_int(w_object))
    elif isinstance(w_object, W_BytesObject):
        # if w_object.getclass(space).is_same_object(space.w_String):
        return py_space.newtext(space.unwrap_string(w_object))
    # import pdb; pdb.set_trace()
    print 'Cannot convert %s to Python' % w_object
    raise PrimitiveFailedError


def get_restart_pycode(source, filename='<string>', cmd='exec'):
    print 'Trying to patch:\n%s' % source
    try:
        py_code = py_compiling.compile(py_space, py_space.newtext(source),
                                       filename, cmd)
        if not isinstance(py_code, PyCode):
            print 'py_code not an instance of PyCode'
            return
        if cmd == 'eval':
            return py_code
        co_consts_w_len = len(py_code.co_consts_w)
        if co_consts_w_len >= 1:
            if co_consts_w_len > 1:
                print 'More than 1 const produced: %s' % co_consts_w_len
            first_consts_w = py_code.co_consts_w[0]
            if not isinstance(first_consts_w, PyCode):
                print 'First const is not a PyCode'
                return py_code
            return first_consts_w
    except OperationError as e:
        # import pdb; pdb.set_trace()
        print 'Failed to compile new frame: %s' % e.errorstr(py_space)
    return


def operr_to_pylist(operr):
    if not isinstance(operr, OperationError):
        return
    wp_exception = py_space.newtext(operr.w_type.getname(py_space))
    wp_value = operr.get_w_value(py_space)
    # wp_traceback = operr.get_traceback() or py_space.w_None
    return py_space.newlist([wp_exception, wp_value])  # wp_traceback])


def entry_point(argv):
    from rsqueakvm.main import safe_entry_point
    if '--python' in argv:
        filename = argv[-1]
        if not os.path.isfile(filename):
            print 'File "%s" does not exist.' % filename
            return 1
        with open(filename, 'r') as f:
            runstring = f.read()
            # import pdb; pdb.set_trace()
            _run_eval_string(runstring, filename, 'exec')
        return 0
    return safe_entry_point(argv)
