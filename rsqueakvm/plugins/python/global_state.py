from rsqueakvm.error import Exit
from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.plugins.foreign_language import global_state as fl_gs
from rsqueakvm.plugins.python.py_objspace import new_pypy_objspace
from rsqueakvm.util.cells import QuasiConstant, Cell

from pypy.interpreter.error import OperationError
from pypy.interpreter.executioncontext import PeriodicAsyncAction


class RestartException(OperationError):
    def __init__(self, py_frame_restart_info):
        self.py_frame_restart_info = py_frame_restart_info

    def _compute_value(self, space):
        print '_compute_value called in RestartException'
        return None


class PyFrameRestartInfo():
    def __init__(self, frame=None, code=None):
        self.frame = frame
        self.pycode = code


class SwitchToSmalltalkAction(PeriodicAsyncAction):

    def __init__(self, py_space):
        PeriodicAsyncAction.__init__(self, py_space)

    def perform(self, ec, frame):
        # import pdb; pdb.set_trace()
        language = ec.current_language
        if language is None:
            return
        runner = language.runner()
        if runner is None:
            return

        # print 'Python yield'
        runner.return_to_smalltalk()
        # print 'Python continue'

        # operror has been in Smalltalk land, clear it now to allow resuming
        language.reset_error()

        # handle py_frame_restart_info if set
        restart_info = py_frame_restart_info.get()
        if restart_info is not None:
            py_frame_restart_info.set(None)
            # import pdb; pdb.set_trace()
            raise RestartException(restart_info)


py_space = new_pypy_objspace()
switch_action = SwitchToSmalltalkAction(py_space)
py_space.actionflag.register_periodic_action(switch_action,
                                             use_bytecode_counter=True)
py_frame_restart_info = Cell(None, type=PyFrameRestartInfo)

break_on_exception = Cell(True)

w_foreign_language_class = QuasiConstant(None, type=W_PointersObject)
w_python_resume_method = QuasiConstant(None, type=W_CompiledMethod)
w_python_class = QuasiConstant(None, type=W_PointersObject)
w_python_object_class = QuasiConstant(None, type=W_PointersObject)
w_python_plugin_send = QuasiConstant(None, type=W_PointersObject)


def startup(space):
    fl_gs.startup(space)

    w_python_plugin_send.set(space.wrap_list_unroll_safe([
        space.wrap_string('PythonPlugin'),
        space.wrap_string('send')
    ]))

    python_class = space.smalltalk_at('Python')
    if python_class is None:
        # disable plugin?
        error_msg = 'Python class not found.'
        print error_msg
        raise Exit(error_msg)
    w_python_class.set(python_class)

    python_object_class = space.smalltalk_at('PythonObject')
    if python_object_class is None:
        # disable plugin?
        error_msg = 'PythonObject class not found.'
        print error_msg
        raise Exit(error_msg)
    w_python_object_class.set(python_object_class)

    resume_method_symbol = space.wrap_symbol('resume:')
    python_cls_cls_s = python_class.getclass(space).as_class_get_shadow(space)
    resume_method = python_cls_cls_s.lookup(resume_method_symbol)
    if resume_method is None:
        # disable plugin?
        error_msg = 'Python class>>resumeFrame method not found.'
        print error_msg
        raise Exit(error_msg)
    w_python_resume_method.set(resume_method)
