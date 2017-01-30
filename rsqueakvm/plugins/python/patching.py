from rsqueakvm.util.cells import Cell
from rsqueakvm.plugins.python import global_state as gs
from rsqueakvm.plugins.python.constants import PYTHON_BYTECODES_THRESHOLD

from pypy.interpreter.error import OperationError
from pypy.module.pypyjit.interp_jit import PyFrame, PyPyJitDriver

from rsqueakvm.plugins.python.patched_dispatch_bytecode import (
    dispatch_bytecode)


python_interrupt_counter = Cell(PYTHON_BYTECODES_THRESHOLD)


class RestartException(OperationError):
    def __init__(self, py_frame_restart_info):
        self.py_frame_restart_info = py_frame_restart_info

    def _compute_value(self, space):
        print '_compute_value called in RestartException'
        return None


def _return_to_smalltalk():
    runner = gs.py_runner.get()
    if runner:
        print 'Python yield'
        runner.return_to_smalltalk()
        print 'Python continue'


def check_for_interrupts():
    new_pic = python_interrupt_counter.get() - 1
    python_interrupt_counter.set(new_pic)
    if new_pic <= 0:
        python_interrupt_counter.set(PYTHON_BYTECODES_THRESHOLD)
        _return_to_smalltalk()


def check_frame_restart_info(py_frame):
    py_frame_restart_info = gs.py_frame_restart_info.get()
    if py_frame_restart_info:
        gs.py_frame_restart_info.set(None)
        # import pdb; pdb.set_trace()
        raise RestartException(py_frame_restart_info)


def smalltalk_check(self):
    check_for_interrupts()
    check_frame_restart_info(self)
    return -1  # Let bytecode loop continue as normal

old_execute_frame = PyFrame.execute_frame


def new_execute_frame(self, w_inputvalue=None, operr=None):
    while True:
        try:
            return old_execute_frame(self, w_inputvalue, operr)
        except RestartException as e:
            # import pdb; pdb.set_trace()
            frame = e.py_frame_restart_info.frame
            if frame is not None and frame is not self:
                raise RestartException(e.py_frame_restart_info)
            self.reset(e.py_frame_restart_info.pycode)

old_init_frame = PyFrame.__init__


def __init__frame(self, space, code, w_globals, outer_func):
    self.w_globals = w_globals
    self.outer_func = outer_func
    old_init_frame(self, space, code, w_globals, outer_func)


def reset_frame(self, new_py_code=None):
    # w_inputvalue missing, see execute_frame
    if new_py_code is None:
        new_py_code = self.pycode
    self.__init__(self.space, new_py_code, self.w_globals, self.outer_func)
    self.last_instr = -1

old_handle_operation_error = PyFrame.handle_operation_error


def new_handle_operation_error(self, ec, operr, attach_tb=True):
    if not isinstance(operr, RestartException):
        gs.wp_error.set(operr.get_w_value(gs.py_space))
        print "Python error caught"
    else:
        print "RestartException skipped"
    _return_to_smalltalk()
    # import pdb; pdb.set_trace()
    check_frame_restart_info(self)
    return old_handle_operation_error(self, ec, operr, attach_tb)


def patch_pypy():
    # Patch-out virtualizables from Pypy so that translation works
    try:
        # TODO: what if first delattr fails?
        delattr(PyFrame, "_virtualizable_")
        delattr(PyPyJitDriver, "virtualizables")
    except AttributeError:
        pass

    PyFrame.smalltalk_check = smalltalk_check
    PyFrame.dispatch_bytecode = dispatch_bytecode

    PyFrame.__init__ = __init__frame
    PyFrame.execute_frame = new_execute_frame
    PyFrame.reset = reset_frame
    PyFrame.handle_operation_error = new_handle_operation_error
