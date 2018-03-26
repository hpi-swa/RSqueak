from rsqueakvm.plugins.ruby.model import W_RubyObject
from rsqueakvm.plugins.ruby.process import W_RubyProcess
from rsqueakvm.plugins.ruby.switching import (
    interrupt_counter, switch_to_smalltalk)
from rsqueakvm.util.cells import QuasiConstant

from topaz import frame as topaz_frame
from topaz.interpreter import (
    ApplicationException, Interpreter as TopazInterpreter)
from topaz.objects.codeobject import W_CodeObject
from topaz.objspace import ObjectSpace as TopazObjectSpace

from rpython.rlib import jit

old_handle_bytecode = TopazInterpreter.handle_bytecode
old_handle_ruby_error = TopazInterpreter.handle_ruby_error
old_jump = TopazInterpreter.jump
old_compile = TopazObjectSpace.compile
old_getexecutioncontext = TopazObjectSpace.getexecutioncontext


def base_frame_get_code_source(self):
    raise NotImplementedError


def frame_get_code_source(self):
    return self.bytecode.source


def builtin_frame_get_code_source(self):
    return self.name


def block_handles_exception(self, block, error_type):
    # import pdb; pdb.set_trace()
    return False


def has_exception_handler(self, error):
    "Returns True if this frame or one of its parents are able to handle operr"
    frame = self
    while frame is not None:
        if isinstance(frame, topaz_frame.Frame):  # skip BaseFrames
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


def annotate_code_object(w_code, source):
    if not isinstance(w_code, W_CodeObject):
        return
    w_code.source = source
    for const_w in w_code.consts_w:
        annotate_code_object(const_w, source)


def new_compile(self, source, filepath, initial_lineno=1, symtable=None):
    bc = old_compile(self, source, filepath, initial_lineno, symtable)
    annotate_code_object(bc, source)
    return bc


def new_getexecutioncontext(self):
    current_ruby_process = self.current_ruby_process.get()
    if current_ruby_process is not None:
        return current_ruby_process.ec
    return old_getexecutioncontext(self)


def patch_topaz():
    # Patch-out virtualizables from Topaz so that translation works
    try:
        delattr(topaz_frame.Frame, "_virtualizable_")
        delattr(TopazInterpreter.jitdriver, "virtualizables")
    except AttributeError:
        pass  # this is fine

    W_CodeObject._immutable_fields_.append('source')
    topaz_frame.BaseFrame.get_code_source = base_frame_get_code_source
    topaz_frame.Frame.get_code_source = frame_get_code_source
    topaz_frame.BuiltinFrame.get_code_source = builtin_frame_get_code_source

    topaz_frame.Frame.has_exception_handler = has_exception_handler
    topaz_frame.Frame.block_handles_exception = block_handles_exception
    TopazInterpreter.handle_bytecode = new_handle_bytecode
    TopazInterpreter.handle_ruby_error = new_handle_ruby_error
    TopazInterpreter.jump = new_jump

    TopazObjectSpace.compile = new_compile
    TopazObjectSpace.current_ruby_process = QuasiConstant(None, W_RubyProcess)
    TopazObjectSpace.getexecutioncontext = new_getexecutioncontext
