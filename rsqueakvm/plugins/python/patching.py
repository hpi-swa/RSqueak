from rsqueakvm.util.cells import Cell
from rsqueakvm.plugins.python.global_state import py_runner
from rsqueakvm.plugins.python.constants import PYTHON_BYTECODES_THRESHOLD


from rpython.rlib import objectmodel

from pypy.interpreter.executioncontext import (
    ExecutionContext, TICK_COUNTER_STEP)


python_interrupt_counter = Cell(PYTHON_BYTECODES_THRESHOLD)


def check_for_interrupts():
    new_pic = python_interrupt_counter.get() - 1
    python_interrupt_counter.set(new_pic)
    if new_pic <= 0:
        python_interrupt_counter.set(PYTHON_BYTECODES_THRESHOLD)
        runner = py_runner.get()
        if runner:
            print 'Python yield'
            runner.return_to_smalltalk()
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


def patch_pypy():
    # Patch-out virtualizables from Pypy so that translation works
    from pypy.module.pypyjit.interp_jit import PyFrame, PyPyJitDriver
    try:
        # TODO: what if first delattr fails?
        delattr(PyFrame, "_virtualizable_")
        delattr(PyPyJitDriver, "virtualizables")
    except AttributeError:
        pass

    ExecutionContext.bytecode_trace = new_bytecode_trace
    ExecutionContext.bytecode_only_trace = new_bytecode_only_trace
