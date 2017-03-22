from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.foreign_language.process import W_ForeignLanguageProcess
from rsqueakvm.plugins.python.model import W_PythonObject
from rsqueakvm.plugins.python.objspace import py_space
from rsqueakvm.plugins.python.utils import _run_eval_string, operr_to_pylist

from pypy.interpreter.error import OperationError


class W_PythonProcess(W_ForeignLanguageProcess):
    _attrs_ = ['source', 'filename', 'cmd', 'ec']
    repr_classname = 'W_PythonProcess'

    def __init__(self, source, filename, cmd, break_on_exceptions=True):
        W_ForeignLanguageProcess.__init__(self, break_on_exceptions)
        self.source = source
        self.filename = filename
        self.cmd = cmd
        self.ec = py_space.createexecutioncontext()

    def run(self):
        print 'Python start'
        try:
            # switch back to Squeak before executing Python code
            self.runner().return_to_smalltalk()

            retval = _run_eval_string(self.source, self.filename, self.cmd)
            self.set_result(retval)
        except OperationError as operr:
            # operr was not handled by users, because they pressed proceed.
            # save Python error as result instead.
            self.set_result(operr_to_pylist(operr))

    def set_current(self):
        py_space.current_python_process.set(self)

    def set_result(self, wp_result):
        self.w_result = W_PythonObject(wp_result)

    def set_error(self, wp_operr):
        self.w_error = W_PythonObject(operr_to_pylist(wp_operr))

    def top_w_frame(self):
        if self.ec is None:
            raise PrimitiveFailedError
        topframe = self.ec.gettopframe()
        if topframe is None:
            raise PrimitiveFailedError
        return W_PythonObject(topframe)
