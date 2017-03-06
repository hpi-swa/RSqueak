from rsqueakvm.util import system
if 'PythonPlugin' not in system.optional_plugins:
    raise LookupError
else:
    system.translationconfig.set(thread=True)
    system.translationconfig.set(continuation=True)

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.plugin import Plugin, PluginStartupScripts
from rsqueakvm.plugins.python import model, global_state, utils
from rsqueakvm.plugins.python.global_state import py_space
from rsqueakvm.plugins.python.model import W_PythonObject
from rsqueakvm.plugins.python.patching import patch_pypy

from pypy.interpreter.error import OperationError
from pypy.interpreter.function import (Function, Method, StaticMethod,
                                       ClassMethod)

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


@PythonPlugin.expose_primitive(unwrap_spec=[object, str, str, str],
                               result_is_new_frame=True)
def eval(interp, s_frame, w_rcvr, source, filename, cmd):
    from rsqueakvm.plugins.python import execution
    # Reset error and state
    global_state.wp_result.set(None)
    global_state.wp_operror.set(None)
    # import pdb; pdb.set_trace()
    execution.start_new_thread(
        source, filename, cmd, translated=PythonPlugin.we_are_translated())
    # when we are here, the Python process has yielded
    return execution.switch_to_smalltalk(interp, s_frame, first_call=True)


@PythonPlugin.expose_primitive(unwrap_spec=[object], result_is_new_frame=True)
def resumePython(interp, s_frame, w_rcvr):
    from rsqueakvm.plugins.python import execution
    # print 'Smalltalk yield'
    # import pdb; pdb.set_trace()
    if not execution.resume_thread():
        raise PrimitiveFailedError
    return execution.switch_to_smalltalk(interp, s_frame)


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def lastResult(interp, s_frame, w_rcvr):
    return W_PythonObject(global_state.wp_result.get())


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def lastError(interp, s_frame, w_rcvr):
    operr = global_state.wp_operror.get()
    if operr is None:
        raise PrimitiveFailedError
    return W_PythonObject(utils.operr_to_pylist(operr))


@PythonPlugin.expose_primitive(clean_stack=False)
def breakOnExceptions(interp, s_frame, argcount):
    if argcount == 0:
        return interp.space.wrap_bool(global_state.break_on_exception.get())
    if argcount != 1:
        raise PrimitiveFailedError
    state = s_frame.pop() is interp.space.w_true
    global_state.break_on_exception.set(state)
    return interp.space.wrap_bool(state)


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def getTopFrame(interp, s_frame, w_rcvr):
    # import pdb; pdb.set_trace()
    topframe = py_space.getexecutioncontext().gettopframe()
    if topframe is None:
        raise PrimitiveFailedError
    return W_PythonObject(topframe)


@PythonPlugin.expose_primitive(unwrap_spec=[object, object, str, str, str])
def restartSpecificFrame(interp, s_frame, w_rcvr, w_frame, source, filename,
                         cmd):
    frame = None
    if isinstance(w_frame, model.W_PythonObject):
        frame = w_frame.wp_object
    py_code = None
    if source:
        py_code = utils.get_restart_pycode(source, filename, cmd)
        if py_code is None:
            return interp.space.w_false  # Raising prim error causes crashes
    global_state.py_frame_restart_info.set(
        global_state.PyFrameRestartInfo(frame=frame, code=py_code))
    return interp.space.w_true


@PythonPlugin.expose_primitive(unwrap_spec=[object])
def asSmalltalk(interp, s_frame, w_rcvr):
    if not isinstance(w_rcvr, model.W_PythonObject):
        raise PrimitiveFailedError
    w_result = utils.python_to_smalltalk(interp.space, w_rcvr.wp_object)
    return w_result


@PythonPlugin.expose_primitive(compiled_method=True)
@jit.unroll_safe
def send(interp, s_frame, argcount, w_method):
    # import pdb; pdb.set_trace()
    space = interp.space
    args_w = s_frame.peek_n(argcount)
    wp_rcvr = utils.smalltalk_to_python(space, s_frame.peek(argcount))
    w_selector_name = w_method.literalat0(space, 2)
    if not isinstance(w_selector_name, W_BytesObject):
        print 'w_selector_name not an instance of W_BytesObject'
        raise PrimitiveFailedError
    methodname = space.unwrap_string(w_selector_name)
    idx = methodname.find(':')
    if idx > 0:
        methodname = methodname[0:idx]
    wp_result = None
    try:
        py_attr = py_space.getattr(wp_rcvr, py_space.newtext(methodname))
        if (isinstance(py_attr, Function) or
                isinstance(py_attr, Method) or
                isinstance(py_attr, StaticMethod) or
                isinstance(py_attr, ClassMethod)):
            wp_result = utils.call_method(
                space, wp_rcvr, methodname, args_w)
        else:
            if len(args_w) == 1:
                wp_value = utils.smalltalk_to_python(space, args_w[0])
                py_space.setattr(wp_rcvr, py_space.newtext(methodname),
                                 wp_value)
                wp_result = py_space.w_None
            else:
                wp_result = py_attr
    except OperationError as operationerr:
        print 'Operror in send (%s)' % operationerr.errorstr(py_space)
        raise PrimitiveFailedError
    except Exception as e:
        print 'Unable to call %s on %s: %s' % (methodname, wp_rcvr, e)
        raise PrimitiveFailedError
    if wp_result is None:
        # import pdb; pdb.set_trace()
        print ('Result failure in send primitive (wp_rcvr: %s, methodname: %s)'
               % (wp_rcvr, methodname))
        raise PrimitiveFailedError
    s_frame.pop_n(argcount + 1)
    return W_PythonObject(wp_result)
