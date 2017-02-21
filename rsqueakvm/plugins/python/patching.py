from rsqueakvm.plugins.python import global_state as gs

from pypy.interpreter.pyopcode import SApplicationException
from pypy.module.pypyjit.interp_jit import PyFrame, PyPyJitDriver
from pypy.tool.stdlib_opcode import bytecode_spec

from rpython.rlib.rarithmetic import intmask

opcodedesc = bytecode_spec.opcodedesc

old_init_frame = PyFrame.__init__
old_execute_frame = PyFrame.execute_frame
old_handle_operation_error = PyFrame.handle_operation_error


def __init__frame(self, space, code, w_globals, outer_func):
    old_init_frame(self, space, code, w_globals, outer_func)
    self.w_globals = w_globals
    self.outer_func = outer_func


def new_execute_frame(self, w_inputvalue=None, operr=None):
    try:
        return old_execute_frame(self, w_inputvalue, operr)
    except gs.RestartException as e:
        frame = e.py_frame_restart_info.frame
        if frame is not None and frame is not self:
            raise gs.RestartException(e.py_frame_restart_info)
        # Generate and execute new frame
        new_frame = gs.py_space.FrameClass(
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
        print "Unknown case; expected DUP_TOP"
        return True  # unknown, so assume it handles exception
    next_opcode_idx = block.handlerposition + 1
    next_opcode = ord(self.pycode.co_code[next_opcode_idx])
    if next_opcode == opcodedesc.LOAD_GLOBAL.index:
        # check for multiple LOAD_GLOBALs
        while next_opcode == opcodedesc.LOAD_GLOBAL.index:
            global_index = ord(self.pycode.co_code[next_opcode_idx + 1])
            exception = self._load_global(self.getname_u(global_index))
            if gs.py_space.exception_match(operr_type,
                                           w_check_class=exception):
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
            if gs.py_space.exception_match(operr_type,
                                           w_check_class=exception):
                return True
            next_opcode_idx = next_opcode_idx + 3
            next_opcode = ord(self.pycode.co_code[next_opcode_idx])
        return False
    return False


def has_exception_handler(self, operr):
    "Returns True if this frame or one of his parents are able to handle operr"
    frame = self
    while frame is not None:
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
    if isinstance(operr, gs.RestartException):
        print 'Re-raising RestartException'
        raise operr
    if not self.has_exception_handler(operr):
        # import pdb; pdb.set_trace()
        gs.wp_operror.set(operr)
        print 'Python error caught'
        gs.switch_action.perform()
    return old_handle_operation_error(self, ec, operr, attach_tb)


def patch_pypy():
    # Patch-out virtualizables from PyPy so that translation works
    try:
        # TODO: what if first delattr fails?
        delattr(PyFrame, '_virtualizable_')
        delattr(PyPyJitDriver, 'virtualizables')
    except AttributeError:
        pass

    PyFrame.__init__ = __init__frame
    PyFrame.execute_frame = new_execute_frame
    PyFrame.block_handles_exception = block_handles_exception
    PyFrame.has_exception_handler = has_exception_handler
    PyFrame.handle_operation_error = new_handle_operation_error
