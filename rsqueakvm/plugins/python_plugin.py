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

from pypy.interpreter.baseobjspace import W_Root
from pypy.interpreter.error import OperationError

from rpython.rlib import objectmodel, jit

_DO_NOT_RELOAD = True


def _new_pypy_objspace():
    import sys

    # This module is reloaded, but pypy_getudir has already been deleted
    from pypy.module import sys as pypy_sys
    reload(pypy_sys)
    # if 'pypy_getudir' not in Module.interpleveldefs:
    #     Module.interpleveldefs['pypy_getudir'] = 'foo'

    from pypy.config.pypyoption import get_pypy_config
    translating = sys.argv[0] != '.build/run.py'  # make better
    pypy_config = get_pypy_config(translating=translating)
    from pypy.config.pypyoption import enable_allworkingmodules
    from pypy.config.pypyoption import enable_translationmodules
    enable_allworkingmodules(pypy_config)
    enable_translationmodules(pypy_config)

    # pypy_config.translation.check_str_without_nul = True

    # ensures pypy_hooks has a .space
    pypy_config.objspace.usemodules.pypyjit = True

    # PyPy needs threads
    pypy_config.translation.thread = True
    pypy_config.translation.continuation = True

    # pypy_config.objspace.usemodules.pyexpat = False

    # Python objectspace ctor is not Rpython so create it here and
    # encapsulate it inside the entry point with a closure.
    from pypy.objspace.std import StdObjSpace as PyStdObjSpace
    return PyStdObjSpace(pypy_config)
python_space = _new_pypy_objspace()

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
    python_space.startup()
PluginStartupScripts.append(startup)


@objectmodel.specialize.argtype(0)
def wrap(interp, wp_object):
    # import pdb; pdb.set_trace()
    space = interp.space
    if isinstance(wp_object, WP_FloatObject):
        return space.wrap_float(python_space.float_w(wp_object))
    elif isinstance(wp_object, WP_IntObject):
        return space.wrap_int(python_space.int_w(wp_object))
    elif isinstance(wp_object, WP_BytesObject):
        return space.wrap_string(python_space.str_w(wp_object))
    elif isinstance(wp_object, WP_ListObject):
        return space.wrap_list(
            [wrap(interp, item) for item in wp_object.getitems()])
    elif isinstance(wp_object, WP_NoneObject):
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
        return python_space.newfloat(space.unwrap_float(w_object))
    elif isinstance(w_object, W_SmallInteger):
        return python_space.newint(space.unwrap_int(w_object))
    elif isinstance(w_object, W_BytesObject):
        # if w_object.getclass(space).is_same_object(space.w_String):
        return python_space.newbytes(space.unwrap_string(w_object))
    raise PrimitiveFailedError


class W_PythonObject(W_AbstractObjectWithIdentityHash):
    _attrs_ = ["wp_object", "s_class"]
    _immutable_fields_ = ["wp_object", "s_class?"]
    repr_classname = "W_PythonObject"

    def __init__(self, wp_object):
        self.wp_object = wp_object
        self.s_class = None

    def getclass(self, space):
        return W_PythonObject(self.wp_object.getclass(python_space))

    def class_shadow(self, space):
        wp_class = python_space.type(self.wp_object)
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


@PythonPlugin.expose_primitive(unwrap_spec=[object, str])
def eval(interp, s_frame, w_rcvr, source):
    try:
        fake_globals = python_space.newdict()
        fake_locals = python_space.newdict()
        pycode = main.compilecode(python_space, source, '<string>', 'eval')
        retval = pycode.exec_code(python_space, fake_globals, fake_locals)
        return wrap(interp, retval)
    except OperationError as operationerr:
        print operationerr.errorstr(python_space)
        raise PrimitiveFailedError


def _call_method(interp, wp_rcvr, methodname, args_w):
    args_w_len = len(args_w)
    if args_w_len == 1:
        arg1 = unwrap(interp, args_w[0])
        return wrap(interp, python_space.call_method(wp_rcvr, methodname, arg1))
    elif args_w_len == 2:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        return wrap(interp, python_space.call_method(wp_rcvr, methodname, arg1, arg2))
    elif args_w_len == 3:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        return wrap(interp, python_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3))
    elif args_w_len == 4:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        return wrap(interp, python_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4))
    elif args_w_len == 5:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        arg5 = unwrap(interp, args_w[4])
        return wrap(interp, python_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5))
    elif args_w_len == 6:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        arg5 = unwrap(interp, args_w[4])
        arg6 = unwrap(interp, args_w[5])
        return wrap(interp, python_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5, arg6))
    elif args_w_len == 7:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        arg5 = unwrap(interp, args_w[4])
        arg6 = unwrap(interp, args_w[5])
        arg7 = unwrap(interp, args_w[6])
        return wrap(interp, python_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
    elif args_w_len == 8:
        arg1 = unwrap(interp, args_w[0])
        arg2 = unwrap(interp, args_w[1])
        arg3 = unwrap(interp, args_w[2])
        arg4 = unwrap(interp, args_w[3])
        arg5 = unwrap(interp, args_w[4])
        arg6 = unwrap(interp, args_w[5])
        arg7 = unwrap(interp, args_w[6])
        arg8 = unwrap(interp, args_w[7])
        return wrap(interp, python_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
    return wrap(interp, python_space.call_method(wp_rcvr, methodname))

@PythonPlugin.expose_primitive(compiled_method=True)
@jit.unroll_safe
def send(interp, s_frame, argcount, w_method):
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
        print operationerr.errorstr(python_space)
        raise PrimitiveFailedError
