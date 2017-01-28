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


def smalltalk_check(self):
    check_for_interrupts()

    if gs.restart_frame.get():
        gs.restart_frame.set(False)
        self.reset()
        return 0  # Restart by setting next_instr to 0
    return -1  # Let bytecode loop continue as normal

old_handle_operation_error = PyFrame.handle_operation_error


def new_handle_operation_error(self, ec, operr, attach_tb=True):
    gs.wp_error.set(operr.get_w_value(gs.py_space))
    print "Python error caught"
    _return_to_smalltalk()
    # import pdb; pdb.set_trace()

    if gs.restart_frame.get():
        gs.restart_frame.set(False)
        self.reset()
        return 0  # Restart by setting next_instr to 0

    return old_handle_operation_error(self, ec, operr, attach_tb)


def reset_frame(self):
    ncellvars = len(self.pycode.co_cellvars)
    self.dropvaluesuntil(ncellvars)  # Drop all new values


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

    PyFrame.handle_operation_error = new_handle_operation_error
    PyFrame.reset = reset_frame
