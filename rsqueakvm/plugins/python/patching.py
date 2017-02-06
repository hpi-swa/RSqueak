from rsqueakvm.plugins.python import global_state as gs

from pypy.module.pypyjit.interp_jit import PyFrame, PyPyJitDriver

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
        # import pdb; pdb.set_trace()
        frame = e.py_frame_restart_info.frame
        if frame is not None and frame is not self:
            raise gs.RestartException(e.py_frame_restart_info)
        self.reset(e.py_frame_restart_info.pycode)
        return new_execute_frame(self, w_inputvalue, operr)


def new_handle_operation_error(self, ec, operr, attach_tb=True):
    if isinstance(operr, gs.RestartException):
        print "Re-raising RestartException"
        raise operr
    gs.wp_error.set(operr.get_w_value(gs.py_space))
    print "Python error caught"
    # import pdb; pdb.set_trace()
    gs.switch_action.perform()
    return old_handle_operation_error(self, ec, operr, attach_tb)


def reset_frame(self, new_py_code=None):
    # w_inputvalue missing, see execute_frame
    if new_py_code is None:
        new_py_code = self.pycode
    self.__init__(self.space, new_py_code, self.w_globals, self.outer_func)
    self.last_instr = -1


def patch_pypy():
    # Patch-out virtualizables from Pypy so that translation works
    try:
        # TODO: what if first delattr fails?
        delattr(PyFrame, "_virtualizable_")
        delattr(PyPyJitDriver, "virtualizables")
    except AttributeError:
        pass

    PyFrame.__init__ = __init__frame
    PyFrame.execute_frame = new_execute_frame
    PyFrame.reset = reset_frame
    PyFrame.handle_operation_error = new_handle_operation_error
