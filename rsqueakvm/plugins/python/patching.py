from rsqueakvm.plugins.python.objspace import py_space, switch_action
from rsqueakvm.plugins.python.process import W_PythonProcess
from rsqueakvm.plugins.python.switching import RestartException
from rsqueakvm.plugins.python.utils import operr_to_w_object
from rsqueakvm.util.cells import QuasiConstant

from pypy.interpreter.pycode import PyCode, default_magic
from pypy.interpreter.pyopcode import SApplicationException
from pypy.interpreter.typedef import interp_attrproperty
from pypy.interpreter.pycompiler import PythonAstCompiler
from pypy.module.pypyjit.interp_jit import PyFrame, PyPyJitDriver
from pypy.objspace.std import StdObjSpace as PyStdObjSpace
from pypy.objspace.std.typeobject import TypeCache
from pypy.tool.stdlib_opcode import bytecode_spec

from rpython.rlib.rarithmetic import intmask

opcodedesc = bytecode_spec.opcodedesc

old_init_frame = PyFrame.__init__
old_execute_frame = PyFrame.execute_frame
old_handle_operation_error = PyFrame.handle_operation_error
old_init_pycode = PyCode.__init__
old_getexecutioncontext = PyStdObjSpace.getexecutioncontext
old_compile = PythonAstCompiler.compile
old_compile_ast = PythonAstCompiler.compile_ast

ATTRIBUTE_ERROR_FORBIDDEN_NAMES = ['getattr']
STOP_ITERATION_FORBIDDEN_NAMES = ['next']


def __init__frame(self, space, code, w_globals, outer_func):
    old_init_frame(self, space, code, w_globals, outer_func)
    self.w_globals = w_globals
    self.outer_func = outer_func


def new_execute_frame(self, w_inputvalue=None, operr=None):
    try:
        return old_execute_frame(self, w_inputvalue, operr)
    except RestartException as e:
        frame = e.py_frame_restart_info.frame
        if frame is not None and frame is not self:
            raise RestartException(e.py_frame_restart_info)
        # Generate and execute new frame
        new_frame = py_space.FrameClass(
            self.space, e.py_frame_restart_info.pycode or self.pycode,
            self.w_globals, self.outer_func)
        return new_execute_frame(new_frame, w_inputvalue, operr)


def block_handles_exception(self, block, operr_type):
    "Returns True if block is able to handle operr_type"
    current_opcode = ord(self.pycode.co_code[block.handlerposition])
    if current_opcode == opcodedesc.POP_TOP.index:
        # Check for catch all `except` statement
        if block.handlerposition < 3:  # This cannot succeed, see next line
            return False
        jump_pos = intmask(block.handlerposition) - 3
        prev_opcode = ord(self.pycode.co_code[jump_pos])
        if prev_opcode == opcodedesc.JUMP_FORWARD.index:
            return True
    elif current_opcode != opcodedesc.DUP_TOP.index:
        print "Unknown case; expected DUP_TOP, got: %s" % current_opcode
        return True  # unknown, so assume it handles exception
    next_opcode_idx = block.handlerposition + 1
    next_opcode = ord(self.pycode.co_code[next_opcode_idx])
    if next_opcode == opcodedesc.LOAD_GLOBAL.index:
        # check for multiple LOAD_GLOBALs
        while next_opcode == opcodedesc.LOAD_GLOBAL.index:
            global_index = ord(self.pycode.co_code[next_opcode_idx + 1])
            exception = self._load_global(self.getname_u(global_index))
            if py_space.exception_match(operr_type, w_check_class=exception):
                return True
            next_opcode_idx = next_opcode_idx + 3
            next_opcode = ord(self.pycode.co_code[next_opcode_idx])
        return False
    elif next_opcode == opcodedesc.LOAD_NAME.index:
        # check for multiple LOAD_NAMEs
        while next_opcode == opcodedesc.LOAD_NAME.index:
            nameindex = ord(self.pycode.co_code[next_opcode_idx + 1])
            if self.getorcreatedebug().w_locals is not self.get_w_globals():
                varname = self.getname_u(nameindex)
                exception = self.space.finditem_str(
                    self.getorcreatedebug().w_locals, varname)
            else:  # fall-back
                exception = self._load_global(self.getname_u(nameindex))
            if py_space.exception_match(operr_type, w_check_class=exception):
                return True
            next_opcode_idx = next_opcode_idx + 3
            next_opcode = ord(self.pycode.co_code[next_opcode_idx])
        return False
    return False


def has_exception_handler(self, operr):
    "Returns True if this frame or one of its parents are able to handle operr"
    frame = self
    error_type = operr.w_type.getname(py_space)
    forbidden_names = []
    if error_type == 'AttributeError':
        forbidden_names = ATTRIBUTE_ERROR_FORBIDDEN_NAMES
    elif error_type == 'StopIteration':
        forbidden_names = STOP_ITERATION_FORBIDDEN_NAMES
    while frame is not None:
        for name in forbidden_names:
            if name in frame.pycode._co_names:
                return True
        block = frame.lastblock
        while block is not None:
            # block needs to be an ExceptBlock and able to handle operr
            if ((block.handling_mask & SApplicationException.kind) != 0 and
                    frame.block_handles_exception(block, operr.w_type)):
                return True
            block = block.previous
        frame = frame.f_backref()
    return False


def new_handle_operation_error(self, ec, operr, attach_tb=True):
    if isinstance(operr, RestartException):
        print 'Re-raising RestartException'
        raise operr
    language = self.space.current_python_process.get()
    if (language is not None and language.break_on_exceptions() and
            not self.has_exception_handler(operr)):
        # import pdb; pdb.set_trace()
        language.set_error(operr_to_w_object(operr))
        print 'Python error caught'
        switch_action.perform(ec, self)
    return old_handle_operation_error(self, ec, operr, attach_tb)


def __init__pycode(self, space, argcount, nlocals, stacksize, flags,
                   code, consts, names, varnames, filename,
                   name, firstlineno, lnotab, freevars, cellvars,
                   hidden_applevel=False, magic=default_magic):
    self._co_names = names
    self.co_source = None
    old_init_pycode(self, space, argcount, nlocals, stacksize, flags,
                    code, consts, names, varnames, filename,
                    name, firstlineno, lnotab, freevars, cellvars,
                    hidden_applevel, magic)


def new_getexecutioncontext(self):
    current_python_process = self.current_python_process.get()
    if current_python_process is not None:
        return current_python_process.ec
    return old_getexecutioncontext(self)


def annotate_pycode(w_code, source):
    if not isinstance(w_code, PyCode):
        return
    w_code.co_source = source
    for const_w in w_code.co_consts_w:
        annotate_pycode(const_w, source)


def new_compile(self, source, filename, mode, flags, hidden_applevel=False):
    w_code = old_compile(self, source, filename, mode, flags, hidden_applevel)
    annotate_pycode(w_code, source)
    return w_code


def patch_pypy():
    # Patch-out virtualizables from PyPy so that translation works
    try:
        # TODO: what if first delattr fails?
        delattr(PyFrame, '_virtualizable_')
        delattr(PyPyJitDriver, 'virtualizables')
    except AttributeError:
        pass

    PyStdObjSpace.current_python_process = QuasiConstant(None, W_PythonProcess)
    PyStdObjSpace.getexecutioncontext = new_getexecutioncontext

    PyFrame.__init__ = __init__frame
    PyFrame.execute_frame = new_execute_frame
    PyFrame.block_handles_exception = block_handles_exception
    PyFrame.has_exception_handler = has_exception_handler
    PyFrame.handle_operation_error = new_handle_operation_error

    PyCode.__init__ = __init__pycode
    PyCode._immutable_fields_.extend(['_co_names[*]', 'co_source'])

    # Add app-level `co_source` to PyCode (this is hacky)
    w_code_type = py_space.fromcache(TypeCache).getorbuild(PyCode.typedef)
    w_code_type.dict_w['co_source'] = interp_attrproperty(
        'co_source', cls=PyCode, wrapfn="newtext_or_none")

    PythonAstCompiler.compile = new_compile
