from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.plugins.python.py_objspace import new_pypy_objspace
from rsqueakvm.util.cells import QuasiConstant, Cell

from pypy.interpreter.baseobjspace import W_Root as WP_Root
from pypy.interpreter.error import OperationError
from pypy.interpreter.executioncontext import PeriodicAsyncAction

from rpython.rlib import objectmodel


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
    def perform(self, ec=None, frame=None):
        # import pdb; pdb.set_trace()
        runner = py_runner.get()
        if runner:
            # print 'Python yield'
            runner.return_to_smalltalk()
            # print 'Python continue'
        # Handle py_frame_restart_info if set
        restart_info = py_frame_restart_info.get()
        if restart_info:
            py_frame_restart_info.set(None)
            # import pdb; pdb.set_trace()
            raise RestartException(restart_info)


py_space = new_pypy_objspace()
switch_action = SwitchToSmalltalkAction(py_space)
py_space.actionflag.register_periodic_action(switch_action,
                                             use_bytecode_counter=True)
py_frame_restart_info = Cell(None, type=PyFrameRestartInfo)

wp_result = Cell(None, type=WP_Root)
wp_operror = Cell(None, type=WP_Root)

w_python_resume_method = QuasiConstant(None, type=W_CompiledMethod)
w_python_class = QuasiConstant(None, type=W_PointersObject)
w_python_object_class = QuasiConstant(None, type=W_PointersObject)
w_python_plugin_send = QuasiConstant(None, type=W_PointersObject)

py_runner = Cell(None)

translating = [True]


def startup(space):
    w_python_plugin_send.set(space.wrap_list_unroll_safe([
        space.wrap_string('PythonPlugin'),
        space.wrap_string('send')
    ]))
    python_class = space.smalltalk_at('Python')
    w_python_class.set(
        python_class or space.w_nil.getclass(space)
    )
    w_python_object_class.set(
        space.smalltalk_at('PythonObject') or space.w_nil.getclass(space)
    )
    resume_method_symbol = space.wrap_symbol('resumeFrame')
    python_cls_cls_s = python_class.getclass(space).as_class_get_shadow(space)
    resume_method = python_cls_cls_s.lookup(resume_method_symbol)
    if resume_method is None:
        # disable plugin?
        print 'Python class>>resumeFrame method not found'
    w_python_resume_method.set(resume_method)

    translating[0] = objectmodel.we_are_translated()
