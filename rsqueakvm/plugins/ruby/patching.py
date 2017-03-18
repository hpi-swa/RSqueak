from topaz.executioncontext import ExecutionContext as TopazExecutionContext
from topaz.frame import Frame as TopazFrame
from topaz.interpreter import (
    ApplicationException, Interpreter as TopazInterpreter)

old_handle_bytecode = TopazInterpreter.handle_bytecode
old_handle_ruby_error = TopazInterpreter.handle_ruby_error

SWITCH_COUNTER_SIZE = 1000
switch_counter = [SWITCH_COUNTER_SIZE]


def switch_to_smalltalk(ec):
    # import pdb; pdb.set_trace()
    language = ec.current_language
    if language is None:
        return
    runner = language.runner()
    if runner is None:
        return

    # print 'Ruby yield'
    runner.return_to_smalltalk()
    # print 'Ruby continue'

    # error has been in Smalltalk land, clear it now to allow resuming
    language.reset_error()


def block_handles_exception(self, block, error_type):
    # import pdb; pdb.set_trace()
    return False


def has_exception_handler(self, error):
    "Returns True if this frame or one of its parents are able to handle operr"
    frame = self
    while frame is not None:
        if not isinstance(frame, TopazFrame):  # only TopazFrames have blocks
            print '%s is not a topaz `Frame`.' % frame
            return True
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
    if switch_counter[0] <= 0:
        switch_counter[0] = SWITCH_COUNTER_SIZE
        switch_to_smalltalk(space.getexecutioncontext())
    switch_counter[0] -= 1
    return old_handle_bytecode(self, space, pc, frame, bytecode)


def new_handle_ruby_error(self, space, pc, frame, bytecode, error):
    ec = space.getexecutioncontext()
    language = ec.current_language
    if (language is not None and language.break_on_exceptions() and
            not frame.has_exception_handler(error)):
        language.set_error(error.w_value)
        print 'Ruby error caught'
        switch_to_smalltalk(ec)
    return old_handle_ruby_error(self, space, pc, frame, bytecode, error)


def patch_topaz():
    # Patch-out virtualizables from Topaz so that translation works
    try:
        delattr(TopazFrame, "_virtualizable_")
        delattr(TopazInterpreter.jitdriver, "virtualizables")
    except AttributeError:
        pass  # this is fine

    TopazExecutionContext.current_language = None
    TopazFrame.has_exception_handler = has_exception_handler
    TopazFrame.block_handles_exception = block_handles_exception
    TopazInterpreter.handle_bytecode = new_handle_bytecode
    TopazInterpreter.handle_ruby_error = new_handle_ruby_error
