from rsqueakvm.util import system
if "PythonPlugin" not in system.optional_plugins:
    raise LookupError
else:
    system.translationconfig.set(thread=True)
    system.translationconfig.set(continuation=True)

from multiprocessing import Process, Pipe

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.numeric import W_Float, W_SmallInteger
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod, W_SpurCompiledMethod
from rsqueakvm.plugins.plugin import Plugin, PluginStartupScripts
from rsqueakvm.storage_classes import ClassShadow
from rsqueakvm.storage import AbstractCachingShadow
from rsqueakvm.primitives.constants import EXTERNAL_CALL
from rsqueakvm.util.cells import QuasiConstant, Cell

from pypy.interpreter.baseobjspace import W_Root as WP_Root
from pypy.objspace.std.boolobject import W_BoolObject as WP_BoolObject
from pypy.objspace.std.intobject import W_IntObject as WP_IntObject
from pypy.objspace.std.floatobject import W_FloatObject as WP_FloatObject
from pypy.objspace.std.bytesobject import W_BytesObject as WP_BytesObject
from pypy.objspace.std.noneobject import W_NoneObject as WP_NoneObject
from pypy.objspace.std.listobject import W_ListObject as WP_ListObject
from pypy.objspace.std.tupleobject import W_TupleObject as WP_TupleObject
from pypy.objspace.std.typeobject import W_TypeObject as WP_TypeObject
from pypy.interpreter.function import BuiltinFunction, Function, Method, StaticMethod, ClassMethod
from pypy.interpreter.pycode import PyCode

from pypy.objspace.std.dictmultiobject import W_DictMultiObject


from pypy.module.__builtin__ import compiling as py_compiling

from pypy.interpreter.baseobjspace import W_Root
from pypy.interpreter.error import OperationError

from rpython.rlib import objectmodel, jit

_DO_NOT_RELOAD = True
PRINT_STRING = 'printString'

from rpython.rlib.rstacklet import StackletThread


class SThread(StackletThread):

    def __init__(self, space, ec):
        StackletThread.__init__(self)
        self.space = space
        self.ec = ec


class PythonRunner:
    def __init__(self, source, cmd):
        self.source = source
        self.cmd = cmd
        self.sthread = None
        # self.h1 = Cell(None)
        # self.h2 = Cell(None)

    def start(self):
        self.sthread = StackletThread()
        self.h1 = self.sthread.new(new_stacklet_callback)

    def resume(self):
        self.sthread.switch(self.h1)

    def run_python(self, h):
        self.h2 = h
        print 'Python start'
        wp_source = py_space.wrap(self.source)
        py_code = py_compiling.compile(py_space, wp_source, '<string>', self.cmd)
        wp_result = py_code.exec_code(py_space, py_globals, py_locals)
        PythonPlugin.wp_result.set(wp_result)
        return h


class PythonPluginClass(Plugin):
    _attrs_ = [
        "w_python_object_class",
        "w_python_plugin_send",
        "py_runner",
        "python_interrupt_counter",
        "current_source",
        "current_cmd",
    ]

    def __init__(self):
        Plugin.__init__(self)
        self.py_runner = Cell(None, type=PythonRunner)
        self.python_interrupt_counter = Cell(1000)
        self.current_source = Cell('')
        self.current_cmd = Cell('')
        self.wp_result = Cell(None, type=WP_Root)
        self.w_python_object_class = QuasiConstant(
            None, type=W_PointersObject)
        self.w_python_plugin_send = QuasiConstant(
            None, type=W_PointersObject)

    def start_new_python(self, source, cmd):
        self.py_runner.set(PythonRunner(source, cmd))
        self.py_runner.get().start()

    def resume_python(self):
        py_runner = self.py_runner.get()
        if py_runner is None:
            raise PrimitiveFailedError
        py_runner.resume()


PythonPlugin = PythonPluginClass()


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

from pypy.interpreter.executioncontext import ExecutionContext, TICK_COUNTER_STEP


def check_for_interrupts():
    new_pic = PythonPlugin.python_interrupt_counter.get() - 1
    PythonPlugin.python_interrupt_counter.set(new_pic)
    if new_pic <= 0:
        PythonPlugin.python_interrupt_counter.set(1000)
        py_runner = PythonPlugin.py_runner.get()
        if py_runner:
            print 'Python yield'
            py_runner.sthread.switch(py_runner.h2)
            print 'Python continue'

old_bytecode_trace = ExecutionContext.bytecode_trace
old_bytecode_only_trace = ExecutionContext.bytecode_only_trace

@objectmodel.always_inline
def new_bytecode_trace(self, frame, decr_by=TICK_COUNTER_STEP):
    check_for_interrupts()
    old_bytecode_trace(self, frame, decr_by)

@objectmodel.always_inline
def new_bytecode_only_trace(self, frame):
    check_for_interrupts()
    old_bytecode_only_trace(self, frame)

ExecutionContext.bytecode_trace = new_bytecode_trace
ExecutionContext.bytecode_only_trace = new_bytecode_only_trace


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


class W_PythonObject(W_PointersObject):
    _attrs_ = ["wp_object", "s_class"]
    _immutable_fields_ = ["wp_object", "s_class?"]
    repr_classname = "W_PythonObject"

    def __init__(self, wp_object):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self.wp_object = wp_object
        # self.w_pyID = None
        self.s_class = None

    def at0(self, space, index0):
        # import pdb; pdb.set_trace()
        return space.w_nil

    def atput0(self, space, index0, w_value):
        # import pdb; pdb.set_trace()
        pass

    def fetch(self, space, n0):
        # import pdb; pdb.set_trace()
        return space.w_nil

    def store(self, space, n0, w_value):
        # import pdb; pdb.set_trace()
        pass

    def getclass(self, space):
        return W_PythonObject(self.wp_object.getclass(py_space))

    def class_shadow(self, space):
        wp_class = py_space.type(self.wp_object)
        return PythonClassShadow(space, self.wp_object, wp_class)

    # @staticmethod
    # @jit.elidable
    # def pure_class_shadow(space, wp_class):
    #     return PythonClassShadowCache.setdefault(
    #         wp_class, PythonClassShadow(space, wp_class))

    def is_same_object(self, other):
        return (isinstance(other, W_PythonObject) and
                other.wp_object is self.wp_object)

# PythonClassShadowCache = {}


class PythonClassShadow(ClassShadow):
    _attrs_ = ["wp_object", "wp_class"]
    _immutable_fields_ = ["wp_class"]

    def __init__(self, space, wp_object, wp_class):
        assert isinstance(wp_class, W_Root)
        self.wp_object = wp_object
        self.wp_class = wp_class
        self.name = wp_class.name
        AbstractCachingShadow.__init__(
            self, space, space.w_nil, 0, space.w_nil)

    def changed(self):
        pass  # Changes to Python classes are handled in Python land

    def lookup(self, w_selector):
        w_method = self.make_method(w_selector)
        # import pdb; pdb.set_trace()
        if w_method is None:
            w_po = PythonPlugin.w_python_object_class.get()
            return w_po.as_class_get_shadow(self.space).lookup(w_selector)
        return w_method

    def make_method(self, w_selector):
        # import pdb; pdb.set_trace()
        methodname = self.space.unwrap_string(w_selector)
        idx = methodname.find(":")
        if idx > 0:
            methodname = methodname[0:idx]

        # import pdb; pdb.set_trace()

        py_attr = None
        # py_attr = True if methodname in ['pyID'] else None  # 'printString'
        try:
            if py_attr is None:
                # check instance vars and methods
                py_attr = py_space.getattr(self.wp_object, py_space.wrap(methodname))
            if py_attr is None:
                # check class vars and methods
                py_attr = py_space.getattr(self.wp_class, py_space.wrap(methodname))
        except OperationError:
            pass
        except Exception as e:
            print 'Unable to create method %s: %s' % (methodname, e)
            return None
        if py_attr is None:
            # check builtins
            if self.wp_class is py_space.type(py_space.builtin):
                try:
                    builtin_func = py_space.builtin.get(methodname)
                    if builtin_func is None:
                        return None
                except OperationError:
                    return None
            else:
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
        return wrap(interp.space, retval)
    except OperationError as operationerr:
        print operationerr.errorstr(py_space)
        # import pdb; pdb.set_trace()
        raise PrimitiveFailedError
    except Exception as e:
        print '[Unknown Exception] %s' % e
        # import pdb; pdb.set_trace()
        raise PrimitiveFailedError


@PythonPlugin.expose_primitive(unwrap_spec=[object, str, str])
def evalWithTopFrame(interp, s_frame, w_rcvr, source, cmd):
    try:
        w_glob = py_space.newdict()
        cur_frame = py_space.getexecutioncontext().gettopframe()
        py_space.setitem(w_glob, py_space.wrap('topframe'), py_space.wrap(cur_frame))
        # import pdb; pdb.set_trace()
        wp_source = py_space.wrap(source)
        py_code = py_compiling.compile(py_space, wp_source, '<string>', cmd)
        retval = py_code.exec_code(py_space, py_globals, py_locals)
        return wrap(interp.space, retval)
    except OperationError as operationerr:
        print operationerr.errorstr(py_space)
        # import pdb; pdb.set_trace()
        raise PrimitiveFailedError
    except Exception as e:
        print '[Unknown Exception] %s' % e
        # import pdb; pdb.set_trace()
        raise PrimitiveFailedError


def new_stacklet_callback(h, arg):
    print 'in switchbackonce_callback:', h, arg
    # import pdb; pdb.set_trace()
    return PythonPlugin.py_runner.get().run_python(h)


@PythonPlugin.expose_primitive(unwrap_spec=[object, str, str])
def syncEval(interp, s_frame, w_rcvr, source, cmd):
    # import pdb; pdb.set_trace()
    PythonPlugin.start_new_python(source, cmd)
    # import pdb; pdb.set_trace()
    # sthread = SThread(py_space, py_space.getexecutioncontext())
    # PythonPlugin.sthread.set(sthread)
    # h = sthread.new(run_python)
    # PythonPlugin.pypy_sthread_h.set(h)
    # import pdb; pdb.set_trace()
    return interp.space.w_nil


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def resumePython(interp, s_frame, w_rcvr):
    # import pdb; pdb.set_trace()
    print 'Smalltalk yield'
    PythonPlugin.resume_python()
    print 'Smalltalk continue'
    return interp.space.w_nil


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def lastResult(interp, s_frame, w_rcvr):
    return wrap(interp.space, PythonPlugin.wp_result.get())


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def asSmalltalk(interp, s_frame, w_rcvr):
    # import pdb; pdb.set_trace()
    if not isinstance(w_rcvr, W_PythonObject):
        raise PrimitiveFailedError
    return wrap(interp.space, w_rcvr.wp_object)


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def getTopFrame(interp, s_frame, w_rcvr):
    # import pdb; pdb.set_trace()
    topframe = py_space.getexecutioncontext().gettopframe()
    # assert? primfail?
    return W_PythonObject(topframe)

@PythonPlugin.expose_primitive(unwrap_spec=[object])
def getGlobalsPerFrame(interp, s_frame, w_rcvr):
    cur_frame = py_space.getexecutioncontext().gettopframe()
    # assert? primfail?
    retval = []
    while cur_frame is not None:
        cur_w_globals = cur_frame.get_w_globals()
        # import pdb; pdb.set_trace()
        if cur_w_globals is None or not isinstance(cur_w_globals, W_DictMultiObject):
            continue
        w_values = [wrap(interp.space, x) for x in cur_w_globals.values()]
        w_values_without_pyobjects = [
            x for x in w_values if not isinstance(x, W_PythonObject)]
        retval.append(interp.space.wrap_list(w_values_without_pyobjects))
        cur_frame = cur_frame.f_backref()
    # import pdb; pdb.set_trace()
    return interp.space.wrap_list(retval)


@PythonPlugin.expose_primitive(unwrap_spec=[object, str])
def getGlobal(interp, s_frame, w_rcvr, key):
    # import pdb; pdb.set_trace()
    return wrap(interp.space, py_globals.getitem(py_space.wrap(key)))


@PythonPlugin.expose_primitive(unwrap_spec=[object, str])
def getLocal(interp, s_frame, w_rcvr, key):
    return wrap(interp.space, py_locals.getitem(py_space.wrap(key)))


def _call_method(space, wp_rcvr, methodname, args_w):
    args_w_len = len(args_w)
    if args_w_len == 1:
        arg1 = unwrap(space, args_w[0])
        return wrap(space, py_space.call_method(wp_rcvr, methodname, arg1))
    elif args_w_len == 2:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        return wrap(space, py_space.call_method(wp_rcvr, methodname, arg1, arg2))
    elif args_w_len == 3:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        return wrap(space, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3))
    elif args_w_len == 4:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        return wrap(space, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4))
    elif args_w_len == 5:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        arg5 = unwrap(space, args_w[4])
        return wrap(space, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5))
    elif args_w_len == 6:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        arg5 = unwrap(space, args_w[4])
        arg6 = unwrap(space, args_w[5])
        return wrap(space, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5, arg6))
    elif args_w_len == 7:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        arg5 = unwrap(space, args_w[4])
        arg6 = unwrap(space, args_w[5])
        arg7 = unwrap(space, args_w[6])
        return wrap(space, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
    elif args_w_len == 8:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        arg5 = unwrap(space, args_w[4])
        arg6 = unwrap(space, args_w[5])
        arg7 = unwrap(space, args_w[6])
        arg8 = unwrap(space, args_w[7])
        return wrap(space, py_space.call_method(wp_rcvr, methodname, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
    return wrap(space, py_space.call_method(wp_rcvr, methodname))


def _call_function(space, wp_func, args_w):
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
        return wrap(space, py_space.call_function(wp_func, arg1, arg2, arg3, arg4))
    elif args_w_len == 5:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        arg5 = unwrap(space, args_w[4])
        return wrap(space, py_space.call_function(wp_func, arg1, arg2, arg3, arg4, arg5))
    elif args_w_len == 6:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        arg5 = unwrap(space, args_w[4])
        arg6 = unwrap(space, args_w[5])
        return wrap(space, py_space.call_function(wp_func, arg1, arg2, arg3, arg4, arg5, arg6))
    elif args_w_len == 7:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        arg5 = unwrap(space, args_w[4])
        arg6 = unwrap(space, args_w[5])
        arg7 = unwrap(space, args_w[6])
        return wrap(space, py_space.call_function(wp_func, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
    elif args_w_len == 8:
        arg1 = unwrap(space, args_w[0])
        arg2 = unwrap(space, args_w[1])
        arg3 = unwrap(space, args_w[2])
        arg4 = unwrap(space, args_w[3])
        arg5 = unwrap(space, args_w[4])
        arg6 = unwrap(space, args_w[5])
        arg7 = unwrap(space, args_w[6])
        arg8 = unwrap(space, args_w[7])
        return wrap(space, py_space.call_function(wp_func, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
    return wrap(space, py_space.call_function(wp_func))


@PythonPlugin.expose_primitive(compiled_method=True)
@jit.unroll_safe
def send(interp, s_frame, argcount, w_method):
    space = interp.space
    args_w = s_frame.peek_n(argcount)
    w_literal1 = w_method.literalat0(interp.space, 1)
    methodname = ""
    if w_literal1 is PythonPlugin.w_python_plugin_send.get():
        w_rcvr = s_frame.peek(argcount)
        wp_rcvr = unwrap(space, w_rcvr)
        w_literal2 = w_method.literalat0(interp.space, 2)
        assert isinstance(w_literal2, W_BytesObject)
        methodname = interp.space.unwrap_string(w_literal2)
    elif argcount == 3:
        methodname = interp.space.unwrap_string(args_w[0])
        w_rcvr = args_w[1]
        wp_rcvr = unwrap(space, w_rcvr)
        args_w = interp.space.unwrap_array(args_w[2])
    else:
        raise PrimitiveFailedError
    idx = methodname.find(":")
    if idx > 0:
        methodname = methodname[0:idx]
    # import pdb; pdb.set_trace()
    # if methodname == 'printString':
    #     return space.wrap_string(wp_rcvr.getclass(py_space).getname(py_space))
    # if methodname == 'pyID' and isinstance(w_rcvr, W_PythonObject):
    #     if len(args_w) == 1:
    #         w_rcvr.w_pyID = args_w[0]
    #         return space.w_nil
    #     else:
    #         return w_rcvr.w_pyID
    try:
        if wp_rcvr is py_space.builtin:
            builtin = py_space.builtin.get(methodname)
            if isinstance(builtin, BuiltinFunction):
                return _call_function(space, builtin, args_w)
            if isinstance(builtin, WP_TypeObject):
                if methodname == 'tuple':
                    return wrap(space, py_space.newtuple(
                        [unwrap(space, x) for x in args_w]))
                elif methodname == 'str':
                    if len(args_w) > 0:
                        return args_w[0]
                    return interp.space.wrap_string('')
                elif methodname == 'bool':
                    if len(args_w) > 0:
                        return args_w[0]
                    return interp.space.w_false
                elif methodname == 'int':
                    if len(args_w) > 0:
                        return args_w[0]
                    return interp.space.wrap_int(0)
                elif methodname == 'float':
                    if len(args_w) > 0:
                        return args_w[0]
                    return interp.space.wrap_float(0)
        else:
            py_attr = py_space.getattr(wp_rcvr, py_space.wrap(methodname))
            # Only allow to call certain types (e.g. don't allow __class__() atm)
            if (isinstance(py_attr, Function) or
                    isinstance(py_attr, Method) or
                    isinstance(py_attr, StaticMethod) or
                    isinstance(py_attr, ClassMethod)):
                return _call_method(space, wp_rcvr, methodname, args_w)
            else:
                if len(args_w) == 1:
                    py_space.setattr(wp_rcvr, py_space.wrap(methodname), unwrap(space, args_w[0]))
                    return space.w_nil
                else:
                    return wrap(space, py_attr)
    except OperationError as operationerr:
        print operationerr.errorstr(py_space)
    except Exception as e:
        print 'Unable to call %s on %s: %s' % (methodname, wp_rcvr, e)
    raise PrimitiveFailedError
