from rsqueakvm.plugins.foreign_language.language import W_ForeignLanguage
from rsqueakvm.plugins.python import (
    py_space, w_python_class, w_python_resume_method)
from rsqueakvm.plugins.python.model import W_PythonObject
from rsqueakvm.plugins.python.utils import _run_eval_string, operr_to_pylist

from pypy.interpreter.error import OperationError


class W_PythonLanguage(W_ForeignLanguage):
    _attrs_ = ['source', 'filename', 'cmd']
    repr_classname = 'W_PythonLanguage'

    def __init__(self, source, filename, cmd, break_on_exceptions=True):
        W_ForeignLanguage.__init__(self, break_on_exceptions)
        self.source = source
        self.filename = filename
        self.cmd = cmd
        self.w_result = None
        self.w_error = None

    def run(self):
        print 'Python start'
        try:
            # ensure py_space has a fresh exectioncontext
            py_space.threadlocals.enter_thread(py_space)

            # switch back to Squeak before executing Python code
            self.runner().return_to_smalltalk()

            retval = _run_eval_string(self.source, self.filename, self.cmd)
            self.set_result(retval)
        except OperationError as operr:
            # operr was not handled by users, because they pressed proceed.
            # save Python error as result instead.
            self.set_result(operr_to_pylist(operr))
        except Exception as e:
            print 'Unknown error in Python thread: %s' % e
        finally:
            self.mark_done()

    def set_current(self):
        ec = py_space.getexecutioncontext()
        ec.current_language = self

    def set_result(self, wp_result):
        self.w_result = W_PythonObject(wp_result)

    def set_error(self, wp_operr):
        self.w_error = W_PythonObject(operr_to_pylist(wp_operr))

    def resume_class(self):
        return w_python_class.get()

    def resume_method(self):
        return w_python_resume_method.get()
