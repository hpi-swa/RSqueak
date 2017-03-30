from rsqueakvm.plugins.foreign_language.process import W_ForeignLanguageProcess
from rsqueakvm.plugins.python.model import W_PythonObject
from rsqueakvm.plugins.python.objspace import py_space
from rsqueakvm.plugins.python.utils import _run_eval_string, operr_to_w_object

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
            retval = _run_eval_string(self.source, self.filename, self.cmd)
            self.set_result(W_PythonObject(retval))
        except OperationError as operr:
            # operr was not handled by users, because they pressed proceed.
            # save Python error as result instead.
            self.set_result(operr_to_w_object(operr))

    def pre_resume(self):
        py_space.current_python_process.set(self)

    def post_resume(self):
        # unset `current_python_process` to restore original behavior
        py_space.current_python_process.set(None)

    def w_top_frame(self):
        if self.ec is None:
            return None
        topframe = self.ec.gettopframe()
        if topframe is None:
            return None
        return W_PythonObject(topframe)
