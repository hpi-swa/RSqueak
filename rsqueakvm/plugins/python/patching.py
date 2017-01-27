from rsqueakvm.util.cells import Cell
from rsqueakvm.plugins.python.global_state import py_runner, wp_error, py_space
from rsqueakvm.plugins.python.constants import PYTHON_BYTECODES_THRESHOLD

from pypy.interpreter.executioncontext import (
    ExecutionContext, TICK_COUNTER_STEP)
from pypy.module.pypyjit.interp_jit import PyFrame, PyPyJitDriver

from rpython.rlib import objectmodel


python_interrupt_counter = Cell(PYTHON_BYTECODES_THRESHOLD)


def _return_to_smalltalk():
    runner = py_runner.get()
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

old_bytecode_trace = ExecutionContext.bytecode_trace
old_bytecode_only_trace = ExecutionContext.bytecode_only_trace
old_handle_operation_error = PyFrame.handle_operation_error


@objectmodel.always_inline
def new_bytecode_trace(self, frame, decr_by=TICK_COUNTER_STEP):
    check_for_interrupts()
    old_bytecode_trace(self, frame, decr_by)


@objectmodel.always_inline
def new_bytecode_only_trace(self, frame):
    check_for_interrupts()
    old_bytecode_only_trace(self, frame)


def new_handle_operation_error(self, ec, operr, attach_tb=True):
    wp_error.set(operr.get_w_value(py_space))
    _return_to_smalltalk()
    return old_handle_operation_error(self, ec, operr, attach_tb)


def patch_pypy():
    # Patch-out virtualizables from Pypy so that translation works
    try:
        # TODO: what if first delattr fails?
        delattr(PyFrame, "_virtualizable_")
        delattr(PyPyJitDriver, "virtualizables")
    except AttributeError:
        pass

    ExecutionContext.bytecode_trace = new_bytecode_trace
    ExecutionContext.bytecode_only_trace = new_bytecode_only_trace

    PyFrame.handle_operation_error = new_handle_operation_error
