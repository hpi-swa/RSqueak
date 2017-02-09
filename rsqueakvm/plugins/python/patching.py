from rsqueakvm.plugins.python import global_state as gs

from pypy.module.pypyjit.interp_jit import PyFrame, PyPyJitDriver
from pypy.interpreter.pyopcode import SApplicationException

from pypy.tool.stdlib_opcode import bytecode_spec

opcodedesc = bytecode_spec.opcodedesc

old_init_frame = PyFrame.__init__
old_execute_frame = PyFrame.execute_frame
old_handle_operation_error = PyFrame.handle_operation_error


def __init__frame(self, space, code, w_globals, outer_func):
    self.w_globals = w_globals
    self.outer_func = outer_func
    old_init_frame(self, space, code, w_globals, outer_func)


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


def get_exception(self, handlerposition):
    "Retrieve exception to catch"
    next_opcode = ord(self.pycode.co_code[handlerposition + 1])
    if next_opcode == opcodedesc.LOAD_GLOBAL.index:
        global_index = ord(self.pycode.co_code[handlerposition + 2])
        return self._load_global(self.getname_u(global_index))
    elif next_opcode == opcodedesc.LOAD_NAME.index:
        if self.getorcreatedebug().w_locals is self.get_w_globals():
            global_index = ord(self.pycode.co_code[handlerposition + 2])
            return self._load_global(self.getname_u(global_index))
    return None


def has_exception_handler(self, operr):
    "Returns True if this frame or one of his parents are able to handle operr"
    frame = self
    while frame is not None:
        block = frame.lastblock
        while block is not None:
            if (block.handling_mask & SApplicationException.kind) != 0:
                block_exc = get_exception(frame, block.handlerposition)
                if block_exc is not None:
                    if gs.py_space.exception_match(operr.w_type, block_exc):
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
        gs.wp_error.set(operr.get_w_value(gs.py_space))
        print 'Python error caught'
        gs.switch_action.perform()
    return old_handle_operation_error(self, ec, operr, attach_tb)


def patch_pypy():
    # Patch-out virtualizables from Pypy so that translation works
    try:
        # TODO: what if first delattr fails?
        delattr(PyFrame, '_virtualizable_')
        delattr(PyPyJitDriver, 'virtualizables')
    except AttributeError:
        pass

    PyFrame.__init__ = __init__frame
    PyFrame.execute_frame = new_execute_frame
    PyFrame.get_exception = get_exception
    PyFrame.has_exception_handler = has_exception_handler
    PyFrame.handle_operation_error = new_handle_operation_error
