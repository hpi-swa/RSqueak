from topaz.executioncontext import ExecutionContext as TopazExecutionContext
from topaz.frame import Frame as TopazFrame
from topaz.interpreter import Interpreter as TopazInterpreter

old_handle_bytecode = TopazInterpreter.handle_bytecode

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


def new_handle_bytecode(self, space, pc, frame, bytecode):
    if switch_counter[0] <= 0:
        switch_counter[0] = SWITCH_COUNTER_SIZE
        switch_to_smalltalk(space.getexecutioncontext())
    switch_counter[0] -= 1

    return old_handle_bytecode(self, space, pc, frame, bytecode)


def patch_topaz():
    # Patch-out virtualizables from Topaz so that translation works
    try:
        delattr(TopazFrame, "_virtualizable_")
        delattr(TopazInterpreter.jitdriver, "virtualizables")
    except AttributeError:
        pass  # this is fine

    TopazInterpreter.handle_bytecode = new_handle_bytecode
    TopazExecutionContext.current_language = None
