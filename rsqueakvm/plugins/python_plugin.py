from rsqueakvm.util import system
if "PythonPlugin" not in system.optional_plugins:
    raise LookupError
else:
    system.translationconfig.set(thread=True)
    system.translationconfig.set(continuation=True)

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.numeric import W_Float, W_SmallInteger
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod, W_SpurCompiledMethod
from rsqueakvm.plugins.plugin import Plugin, PluginStartupScripts
from rsqueakvm.storage_classes import ClassShadow
from rsqueakvm.storage import AbstractCachingShadow
from rsqueakvm.primitives.constants import EXTERNAL_CALL
from rsqueakvm.util.cells import QuasiConstant

from pypy.interpreter import main
from pypy.objspace.std.boolobject import W_BoolObject as WP_BoolObject
from pypy.objspace.std.intobject import W_IntObject as WP_IntObject
from pypy.objspace.std.floatobject import W_FloatObject as WP_FloatObject
from pypy.objspace.std.bytesobject import W_BytesObject as WP_BytesObject
from pypy.objspace.std.noneobject import W_NoneObject as WP_NoneObject
from pypy.objspace.std.listobject import W_ListObject as WP_ListObject
from pypy.module.__builtin__ import compiling as py_compiling

from pypy.interpreter.baseobjspace import W_Root
from pypy.interpreter.error import OperationError

from rpython.rlib import objectmodel, jit

_DO_NOT_RELOAD = True


def _new_pypy_objspace():
    import os
    import sys

    # This module is reloaded, but pypy_getudir has already been deleted
    from pypy.module import sys as pypy_sys
    reload(pypy_sys)
    # if 'pypy_getudir' not in Module.interpleveldefs:
    #     Module.interpleveldefs['pypy_getudir'] = 'foo'

    from pypy.config.pypyoption import get_pypy_config
    translating = sys.argv[0] != '.build/run.py'  # make better
    pypy_config = get_pypy_config(translating=translating)

    # cpyext causes a lot of "Undefined symbols for architecture x86_64" errors
    pypy_config.objspace.usemodules.cpyext = False

    # disabling cffi backend for now, it also causes an undefined symbol error
    pypy_config.objspace.usemodules._cffi_backend = False

    from pypy.config.pypyoption import enable_allworkingmodules
    from pypy.config.pypyoption import enable_translationmodules
    enable_allworkingmodules(pypy_config)
    enable_translationmodules(pypy_config)

    # pypy_config.translation.check_str_without_nul = True

    # ensures pypy_hooks has a .space
    pypy_config.objspace.usemodules.pypyjit = True

    pypy_config.translation.continuation = True

    pypy_config.objspace.usemodules.pyexpat = False

    # Copy over some options that should be the same in both configs
    pypy_config.translation.make_jobs = system.translationconfig.make_jobs
    if system.translationconfig.output is not None:
        pypy_config.translation.output = system.translationconfig.output

    # merge_configs(config, pypy_config, "RSqueak", "PyPy")

    # PyPy needs threads
    pypy_config.translation.thread = True

    # Python objectspace ctor is not Rpython so create it here and
    # encapsulate it inside the entry point with a closure.
    from pypy.objspace.std import StdObjSpace as PyStdObjSpace

    py_space = PyStdObjSpace(pypy_config)

    # equivalent to the hack in app_main.py of PyPy, albiet interp-level.
    w_sys = py_space.sys
    w_modnames = w_sys.get("builtin_module_names")
    w_in = py_space.contains(w_modnames, py_space.wrap("__pypy__"))
    if not py_space.is_true(w_in):
        rl = py_space.sys.get("setrecursionlimit")
        py_space.call(rl, py_space.newlist([py_space.wrap(5000)]))

    # Should always be able to import Python modules in CWD.
    w_sys_path = py_space.getattr(w_sys, py_space.wrap("path"))
    py_space.call_method(w_sys_path, 'append', py_space.wrap("."))

    # Set sys.executable in PyPy -- some modules rely upon this existing.
    py_space.setattr(w_sys, py_space.wrap("executable"),
                     py_space.wrap(os.path.abspath(sys.argv[0])))
    return py_space

py_space = _new_pypy_objspace()
py_globals = py_space.newdict()
py_locals = py_space.newdict()

# Patch-out virtualizables from Pypy so that translation works
from pypy.module.pypyjit.interp_jit import PyFrame, PyPyJitDriver
try:
    # TODO: what if first delattr fails?
    delattr(PyFrame, "_virtualizable_")
    delattr(PyPyJitDriver, "virtualizables")
except AttributeError:
    pass


class PythonPluginClass(Plugin):
    _attrs_ = ["w_python_object_class", "w_python_plugin_send"]

    def __init__(self):
        Plugin.__init__(self)
        self.w_python_object_class = QuasiConstant(
            None, type=W_AbstractObjectWithIdentityHash)
        self.w_python_plugin_send = QuasiConstant(
            None, type=W_AbstractObjectWithIdentityHash)
PythonPlugin = PythonPluginClass()


def startup(space, argv):
    PythonPlugin.w_python_plugin_send.set(space.wrap_list_unroll_safe([
        space.wrap_string("PythonPlugin"),
        space.wrap_string("send")
    ]))
    w_python_class = space.smalltalk_at("PythonObject")
    if w_python_class is None:
        w_python_class = space.w_nil.getclass(space)
    PythonPlugin.w_python_object_class.set(w_python_class)
    py_space.startup()
PluginStartupScripts.append(startup)


@objectmodel.specialize.argtype(0)
def wrap(interp, wp_object):
    # import pdb; pdb.set_trace()
    space = interp.space
    if isinstance(wp_object, WP_FloatObject):
        return space.wrap_float(py_space.float_w(wp_object))
    elif isinstance(wp_object, WP_IntObject):
        return space.wrap_int(py_space.int_w(wp_object))
    elif isinstance(wp_object, WP_BytesObject):
        return space.wrap_string(py_space.str_w(wp_object))
    elif isinstance(wp_object, WP_ListObject):
        return space.wrap_list(
            [wrap(interp, item) for item in wp_object.getitems()])
    elif wp_object is None or isinstance(wp_object, WP_NoneObject):
        return space.w_nil
    elif wp_object is WP_BoolObject.w_False:
        return space.w_false
    elif wp_object is WP_BoolObject.w_True:
        return space.w_true
    else:
        return W_PythonObject(wp_object)


@objectmodel.specialize.argtype(0)
def unwrap(interp, w_object):
    # import pdb; pdb.set_trace()
    space = interp.space
    if isinstance(w_object, W_PythonObject):
        return w_object.wp_object
    elif isinstance(w_object, W_Float):
        return py_space.newfloat(space.unwrap_float(w_object))
    elif isinstance(w_object, W_SmallInteger):
        return py_space.newint(space.unwrap_int(w_object))
    elif isinstance(w_object, W_BytesObject):
        # if w_object.getclass(space).is_same_object(space.w_String):
        return py_space.newbytes(space.unwrap_string(w_object))
    raise PrimitiveFailedError


class W_PythonObject(W_AbstractObjectWithIdentityHash):
    _attrs_ = ["wp_object", "s_class"]
    _immutable_fields_ = ["wp_object", "s_class?"]
    repr_classname = "W_PythonObject"

    def __init__(self, wp_object):
        self.wp_object = wp_object
        self.s_class = None

    def getclass(self, space):
        return W_PythonObject(self.wp_object.getclass(py_space))

    def class_shadow(self, space):
        wp_class = py_space.type(self.wp_object)
        return W_PythonObject.pure_class_shadow(space, wp_class)

    @staticmethod
    @jit.elidable
    def pure_class_shadow(space, wp_class):
        return PythonClassShadowCache.setdefault(
            wp_class, PythonClassShadow(space, wp_class))

    def is_same_object(self, other):
        return (isinstance(other, W_PythonObject) and
                other.wp_object is self.wp_object)

PythonClassShadowCache = {}


class PythonClassShadow(ClassShadow):
    _attrs_ = ["wp_class"]
    _immutable_fields_ = ["wp_class"]

    def __init__(self, space, wp_class):
        assert isinstance(wp_class, W_Root)
        self.wp_class = wp_class
        self.name = wp_class.name
        AbstractCachingShadow.__init__(
            self, space, space.w_nil, 0, space.w_nil)

    def changed(self):
        pass  # Changes to Python classes are handled in Python land

    def lookup(self, w_selector):
        w_method = self.make_method(w_selector)
        if w_method is None:
            w_po = PythonPlugin.w_python_object_class.get()
            return w_po.as_class_get_shadow(self.space).lookup(w_selector)
        return w_method

    def make_method(self, w_selector):
        methodname = self.space.unwrap_string(w_selector)
        idx = methodname.find(":")
        if idx > 0:
            methodname = methodname[0:idx]
        python_method = self.wp_class.lookup(methodname)
        if python_method is None:
            return None
        if self.space.is_spur.is_set():
            w_cm = objectmodel.instantiate(W_SpurCompiledMethod)
        else:
            w_cm = objectmodel.instantiate(W_PreSpurCompiledMethod)
        w_cm.header = 0
        w_cm._primitive = EXTERNAL_CALL
        w_cm.literalsize = 2
        w_cm.islarge = False
        w_cm._tempsize = 0
        w_cm.argsize = 0
        w_cm.bytes = []
        w_cm.literals = [
            PythonPlugin.w_python_plugin_send.get(),
            w_selector
        ]
        return w_cm


@PythonPlugin.expose_primitive(unwrap_spec=[object, str, str])
def eval(interp, s_frame, w_rcvr, source, cmd):
    try:
        # import pdb; pdb.set_trace()
        wp_source = py_space.wrap(source)
        py_code = py_compiling.compile(py_space, wp_source, '<string>', cmd)
        retval = py_code.exec_code(py_space, py_globals, py_locals)
        return wrap(interp, retval)
    except OperationError as operationerr:
        print operationerr.errorstr(py_space)
        raise PrimitiveFailedError


@PythonPlugin.expose_primitive(unwrap_spec=[object, str])
def getGlobal(interp, s_frame, w_rcvr, key):
    return wrap(interp, py_globals.getitem(py_space.wrap(key)))


@PythonPlugin.expose_primitive(unwrap_spec=[object, str])
def getLocal(interp, s_frame, w_rcvr, key):
    return wrap(interp, py_locals.getitem(py_space.wrap(key)))


def _call_method(interp, wp_rcvr, methodname, args_w):
    args_w_len = len(args_w)
    if args_w_len == 1:
        arg1 = unwrap(interp, args_w[0])
        return wrap(interp, py_space.call_method(wp_rcvr, methodname, arg1))
    elif args_w_len == 2:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        return wrap(interp, py_space.call_method(wp_rcvr, methodname, arg1, arg2))
    elif args_w_len == 3:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        return wrap(interp, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3))
    elif args_w_len == 4:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        return wrap(interp, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4))
    elif args_w_len == 5:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        arg5 = unwrap(interp, args_w[4])
        return wrap(interp, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5))
    elif args_w_len == 6:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        arg5 = unwrap(interp, args_w[4])
        arg6 = unwrap(interp, args_w[5])
        return wrap(interp, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5, arg6))
    elif args_w_len == 7:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        arg5 = unwrap(interp, args_w[4])
        arg6 = unwrap(interp, args_w[5])
        arg7 = unwrap(interp, args_w[6])
        return wrap(interp, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
    elif args_w_len == 8:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        arg5 = unwrap(interp, args_w[4])
        arg6 = unwrap(interp, args_w[5])
        arg7 = unwrap(interp, args_w[6])
        arg8 = unwrap(interp, args_w[7])
        return wrap(interp, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
    return wrap(interp, py_space.call_method(wp_rcvr, methodname))

@PythonPlugin.expose_primitive(compiled_method=True)
@jit.unroll_safe
def send(interp, s_frame, argcount, w_method):
    # import pdb; pdb.set_trace()
    args_w = s_frame.peek_n(argcount)
    w_literal2 = w_method.literalat0(interp.space, 2)
    methodname = ""
    if isinstance(w_literal2, W_BytesObject):
        wp_rcvr = unwrap(interp, s_frame.peek(argcount))
        methodname = interp.space.unwrap_string(w_literal2)
    elif argcount == 3:
        methodname = interp.space.unwrap_string(args_w[0])
        wp_rcvr = unwrap(interp, args_w[1])
        args_w = interp.space.unwrap_array(args_w[2])
    else:
        raise PrimitiveFailedError
    idx = methodname.find(":")
    if idx > 0:
        methodname = methodname[0:idx]
    try:
        return _call_method(interp, wp_rcvr, methodname, args_w)
    except OperationError as operationerr:
        print operationerr.errorstr(py_space)
        raise PrimitiveFailedError
