from rsqueakvm.plugins.ruby.model import W_RubyObject
from rsqueakvm.plugins.ruby.process import W_RubyProcess
from rsqueakvm.util.cells import QuasiConstant

from topaz.frame import Frame as TopazFrame
from topaz.interpreter import (
    ApplicationException, Interpreter as TopazInterpreter)
from topaz.objspace import ObjectSpace as TopazObjectSpace

from rpython.rlib import jit

old_handle_bytecode = TopazInterpreter.handle_bytecode
old_handle_ruby_error = TopazInterpreter.handle_ruby_error
old_jump = TopazInterpreter.jump
old_getexecutioncontext = TopazObjectSpace.getexecutioncontext


class InterruptCounter:
    counter_size = 10000

    def __init__(self):
        self.reset()

    def reset(self):
        self._counter = self.counter_size

    def triggers(self, decr_by=1):
        self._counter -= decr_by
        if self._counter <= 0:
            self._counter = self.counter_size
            return True
        return False


interrupt_counter = InterruptCounter()


def switch_to_smalltalk(ruby_process):
    # import pdb; pdb.set_trace()
    if ruby_process is None:
        return
    runner = ruby_process.runner()
    if runner is None:
        return

    # print 'Ruby yield'
    runner.return_to_smalltalk()
    # print 'Ruby continue'

    # error has been in Smalltalk land, clear it now to allow resuming
    ruby_process.reset_error()


def block_handles_exception(self, block, error_type):
    # import pdb; pdb.set_trace()
    return False


def has_exception_handler(self, error):
    "Returns True if this frame or one of its parents are able to handle operr"
    frame = self
    while frame is not None:
        if isinstance(frame, TopazFrame):  # skip BaseFrames
            block = frame.lastblock
            while block is not None:
                # block needs to be an ExceptBlock and able to handle operr
                if ((block.handling_mask & ApplicationException.kind) != 0 and
                        frame.block_handles_exception(block, error.w_value)):
                    return True
                block = block.lastblock
        frame = frame.backref()
    return False


def new_handle_bytecode(self, space, pc, frame, bytecode):
    if not jit.we_are_jitted() and interrupt_counter.triggers():
        switch_to_smalltalk(space.current_ruby_process.get())
    return old_handle_bytecode(self, space, pc, frame, bytecode)


def new_handle_ruby_error(self, space, pc, frame, bytecode, error):
    language = space.current_ruby_process.get()
    if (language is not None and language.break_on_exceptions() and
            not frame.has_exception_handler(error)):
        language.set_error(W_RubyObject(error.w_value))
        print 'Ruby error caught'
        switch_to_smalltalk(space.current_ruby_process.get())
    return old_handle_ruby_error(self, space, pc, frame, bytecode, error)


def new_jump(self, space, bytecode, frame, cur_pc, target_pc):
    if target_pc < cur_pc and jit.we_are_jitted():
        trace_length = jit.current_trace_length()
        decr_by = int(trace_length >> 12 | 1)
        if interrupt_counter.triggers(decr_by=decr_by):
            switch_to_smalltalk(space.current_ruby_process.get())
    return old_jump(self, space, bytecode, frame, cur_pc, target_pc)


def new_getexecutioncontext(self):
    current_ruby_process = self.current_ruby_process.get()
    if current_ruby_process is not None:
        return current_ruby_process.ec
    return old_getexecutioncontext(self)


def patch_topaz():
    # Patch-out virtualizables from Topaz so that translation works
    try:
        delattr(TopazFrame, "_virtualizable_")
        delattr(TopazInterpreter.jitdriver, "virtualizables")
    except AttributeError:
        pass  # this is fine

    TopazFrame.has_exception_handler = has_exception_handler
    TopazFrame.block_handles_exception = block_handles_exception
    TopazInterpreter.handle_bytecode = new_handle_bytecode
    TopazInterpreter.handle_ruby_error = new_handle_ruby_error
    TopazInterpreter.jump = new_jump

    TopazObjectSpace.current_ruby_process = QuasiConstant(None, W_RubyProcess)
    TopazObjectSpace.getexecutioncontext = new_getexecutioncontext
