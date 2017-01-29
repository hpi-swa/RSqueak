from rsqueakvm.util import system
if "PythonPlugin" not in system.optional_plugins:
    raise LookupError
else:
    system.translationconfig.set(thread=True)
    system.translationconfig.set(continuation=True)

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.plugin import Plugin, PluginStartupScripts
from rsqueakvm.plugins.python import model, global_state
from rsqueakvm.plugins.python.global_state import (
    py_space, py_globals, py_locals)
from rsqueakvm.plugins.python.patching import patch_pypy
from rsqueakvm.plugins.python.utils import (wrap, unwrap, call_function,
                                            call_method, persist_pysource)

from pypy.interpreter.error import OperationError
from pypy.interpreter.function import (BuiltinFunction, Function, Method,
                                       StaticMethod, ClassMethod)
from pypy.module.__builtin__ import compiling as py_compiling
from pypy.objspace.std.typeobject import W_TypeObject as WP_TypeObject


from rpython.rlib import jit, objectmodel


_DO_NOT_RELOAD = True

patch_pypy()


class PythonPluginClass(Plugin):

    def we_are_translated(self):
        return objectmodel.we_are_translated()

PythonPlugin = PythonPluginClass()


def startup(space, argv):
    global_state.startup(space)
    py_space.startup()
PluginStartupScripts.append(startup)


@PythonPlugin.expose_primitive(unwrap_spec=[object, str, str])
def eval(interp, s_frame, w_rcvr, source, cmd):
    try:
        # import pdb; pdb.set_trace()
        filename = persist_pysource(interp.space, source)
        wp_source = py_space.wrap(source)
        py_code = py_compiling.compile(py_space, wp_source, filename, cmd)
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


@PythonPlugin.expose_primitive(unwrap_spec=[object, str, str],
                               result_is_new_frame=True)
def evalInThread(interp, s_frame, w_rcvr, source, cmd):
    from rsqueakvm.plugins.python import execution

    # import pdb; pdb.set_trace()
    execution.start_new_thread(
        source, persist_pysource(interp.space, source), cmd,
        translated=PythonPlugin.we_are_translated())
    # when we are here, the Python process has yielded
    return execution.switch_to_smalltalk(interp, s_frame, first_call=True)


@PythonPlugin.expose_primitive(unwrap_spec=[object], result_is_new_frame=True)
def resumePython(interp, s_frame, w_rcvr):
    from rsqueakvm.plugins.python import execution
    print 'Smalltalk yield'
    # import pdb; pdb.set_trace()
    if not execution.resume_thread():
        raise PrimitiveFailedError
    return execution.switch_to_smalltalk(interp, s_frame)


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def simpleResume(interp, s_frame, w_rcvr):
    # For shell development/debugging
    from rsqueakvm.plugins.python import execution
    print 'Smalltalk simple yield'
    # import pdb; pdb.set_trace()
    if not execution.resume_thread():
        return interp.space.w_false
    return interp.space.w_true


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def lastResult(interp, s_frame, w_rcvr):
    return wrap(interp.space, global_state.wp_result.get())


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def lastError(interp, s_frame, w_rcvr):
    wp_error = global_state.wp_error.get()
    if wp_error is None:
        raise PrimitiveFailedError
    return model.W_PythonObject(wp_error)


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def asSmalltalk(interp, s_frame, w_rcvr):
    # import pdb; pdb.set_trace()
    if not isinstance(w_rcvr, model.W_PythonObject):
        raise PrimitiveFailedError
    return wrap(interp.space, w_rcvr.wp_object)


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def getTopFrame(interp, s_frame, w_rcvr):
    # import pdb; pdb.set_trace()
    topframe = py_space.getexecutioncontext().gettopframe()
    # assert? primfail?
    return model.W_PythonObject(topframe)


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def restartFrame(interp, s_frame, w_rcvr):
    # import pdb; pdb.set_trace()
    global_state.py_frame_restart_info.set(
        global_state.PyFrameRestartInfo())
    return interp.space.w_true


@PythonPlugin.expose_primitive(unwrap_spec=[object, str, str])
def restartFrameWith(interp, s_frame, w_rcvr, source, cmd):
    filename = persist_pysource(interp.space, source)
    wp_source = py_space.wrap(source)
    try:
        py_code = py_compiling.compile(py_space, wp_source, filename, cmd)
    except:
        print 'Failed to compile new frame'
        raise PrimitiveFailedError
    global_state.py_frame_restart_info.set(
        global_state.PyFrameRestartInfo(code=py_code))
    return interp.space.w_true


@PythonPlugin.expose_primitive(unwrap_spec=[object, object, str, str])
def restartSpecificFrame(interp, s_frame, w_rcvr, w_frame, source, cmd):
    filename = persist_pysource(interp.space, source)
    py_code = None
    if source:
        wp_source = py_space.wrap(source)
        try:
            py_code = py_compiling.compile(py_space, wp_source, filename, cmd)
        except:
            print 'Failed to compile new frame'
            raise PrimitiveFailedError
    frame = None
    if isinstance(w_frame, model.W_PythonObject):
        frame = w_frame.wp_object
    global_state.py_frame_restart_info.set(
        global_state.PyFrameRestartInfo(frame=frame, code=py_code))
    return interp.space.w_true


@PythonPlugin.expose_primitive(unwrap_spec=[object, str])
def getGlobal(interp, s_frame, w_rcvr, key):
    # import pdb; pdb.set_trace()
    return wrap(interp.space, py_globals.getitem(py_space.wrap(key)))


@PythonPlugin.expose_primitive(unwrap_spec=[object, str])
def getLocal(interp, s_frame, w_rcvr, key):
    return wrap(interp.space, py_locals.getitem(py_space.wrap(key)))


@PythonPlugin.expose_primitive(compiled_method=True)
@jit.unroll_safe
def send(interp, s_frame, argcount, w_method):
    # import pdb; pdb.set_trace()
    space = interp.space
    args_w = s_frame.peek_n(argcount)
    wp_rcvr = unwrap(space, s_frame.peek(argcount))
    w_selector_name = w_method.literalat0(space, 2)
    assert isinstance(w_selector_name, W_BytesObject)
    methodname = space.unwrap_string(w_selector_name)
    idx = methodname.find(":")
    if idx > 0:
        methodname = methodname[0:idx]
    w_result = None
    try:
        if wp_rcvr is py_space.builtin:
            builtin = py_space.builtin.get(methodname)
            if isinstance(builtin, BuiltinFunction):
                w_result = call_function(space, builtin, args_w)
            if isinstance(builtin, WP_TypeObject):
                if methodname == 'tuple':
                    w_result = wrap(space, py_space.newtuple(
                        [unwrap(space, x) for x in args_w]))
                elif methodname == 'str':
                    if len(args_w) > 0:
                        w_result = args_w[0]
                    w_result = space.wrap_string('')
                elif methodname == 'bool':
                    if len(args_w) > 0:
                        w_result = args_w[0]
                    w_result = space.w_false
                elif methodname == 'int':
                    if len(args_w) > 0:
                        w_result = args_w[0]
                    w_result = space.wrap_int(0)
                elif methodname == 'float':
                    if len(args_w) > 0:
                        w_result = args_w[0]
                    w_result = space.wrap_float(0)
        else:
            py_attr = py_space.getattr(wp_rcvr, py_space.wrap(methodname))
            # Only allow to call certain types (e.g. don't allow __class__())
            if (isinstance(py_attr, Function) or
                    isinstance(py_attr, Method) or
                    isinstance(py_attr, StaticMethod) or
                    isinstance(py_attr, ClassMethod)):
                w_result = call_method(space, wp_rcvr, methodname, args_w)
            else:
                if len(args_w) == 1:
                    py_space.setattr(wp_rcvr, py_space.wrap(methodname),
                                     unwrap(space, args_w[0]))
                    w_result = space.w_nil
                else:
                    w_result = wrap(space, py_attr)
    except OperationError as operationerr:
        print operationerr.errorstr(py_space)
        raise PrimitiveFailedError
    except Exception as e:
        print 'Unable to call %s on %s: %s' % (methodname, wp_rcvr, e)
        raise PrimitiveFailedError
    if w_result is None:
        print "Unable to get result in send primitive"
        raise PrimitiveFailedError
    s_frame.pop_n(argcount + 1)
    return w_result
