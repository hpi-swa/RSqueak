from rsqueakvm.util.cells import Cell
from rsqueakvm.plugins.python import global_state as gs
from rsqueakvm.plugins.python.constants import PYTHON_BYTECODES_THRESHOLD

from pypy.module.pypyjit.interp_jit import PyFrame, PyPyJitDriver

from rsqueakvm.plugins.python.patched_dispatch_bytecode import (
    dispatch_bytecode)


python_interrupt_counter = Cell(PYTHON_BYTECODES_THRESHOLD)


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


def handle_restart_frame(py_frame):
    gs.restart_frame.set(False)
    py_code = gs.restart_frame_code.get()
    if py_code:
        gs.restart_frame_code.set(None)
    py_frame.reset(py_code)
    return 0  # Restart by setting next_instr to 0


def smalltalk_check(self):
    check_for_interrupts()
    if gs.restart_frame.get():
        return handle_restart_frame(self)
    return -1  # Let bytecode loop continue as normal

old_handle_operation_error = PyFrame.handle_operation_error


def new_handle_operation_error(self, ec, operr, attach_tb=True):
    gs.wp_error.set(operr.get_w_value(gs.py_space))
    print "Python error caught"
    _return_to_smalltalk()
    # import pdb; pdb.set_trace()

    if gs.restart_frame.get():
        return handle_restart_frame(self)

    return old_handle_operation_error(self, ec, operr, attach_tb)

old_init_frame = PyFrame.__init__


def __init__frame(self, space, code, w_globals, outer_func):
    self.w_globals = w_globals
    self.outer_func = outer_func
    old_init_frame(self, space, code, w_globals, outer_func)


def reset_frame(self, new_py_code=None):
    if new_py_code is None:
        new_py_code = self.pycode
    self.__init__(self.space, new_py_code, self.w_globals, self.outer_func)


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
    PyFrame.handle_operation_error = new_handle_operation_error
    PyFrame.reset = reset_frame
