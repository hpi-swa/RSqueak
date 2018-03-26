from rsqueakvm.plugins.foreign_language.process import W_ForeignLanguageProcess
from rsqueakvm.plugins.python.model import W_PythonObject
from rsqueakvm.plugins.python.objspace import py_space
from rsqueakvm.plugins.python import utils

from pypy.interpreter.argument import Arguments
from pypy.interpreter.error import OperationError


class W_PythonProcess(W_ForeignLanguageProcess):
    _attrs_ = ['source', 'filename', 'cmd', 'ec']
    repr_classname = 'W_PythonProcess'

    def __init__(self, space, w_rcvr=None, method_name='', args_w=None,
                 source='', filename='', cmd='',
                 is_send=False, break_on_exceptions=False):
        W_ForeignLanguageProcess.__init__(
            self, space, w_rcvr, method_name, args_w,
            is_send, break_on_exceptions)
        self.source = source
        self.filename = filename
        self.cmd = cmd
        self.ec = py_space.createexecutioncontext()
        self.init_runner()

    def eval(self):
        if self.source == '' or self.filename == '' or self.cmd == '':
            return self.fail('Invalid Python eval')
        try:
            retval = utils._run_eval_string(
                self.source, self.filename, self.cmd)
            self.set_result(W_PythonObject(retval))
        except OperationError as operr:
            # operr was not handled by users, because they pressed proceed.
            # save Python error as result instead.
            self.set_result(utils.operr_to_w_object(operr))

    def send(self):
        st_to_py = utils.smalltalk_to_python
        wp_rcvr = st_to_py(self.space(), self.w_rcvr)
        wp_attrname = py_space.newtext(self.method_name)
        try:
            if self.method_name == '__call__':  # special __call__ case
                w_meth = py_space.getattr(wp_rcvr, wp_attrname)
                args_wp = [st_to_py(self.space(), a) for a in self.args_w]
                # use call_args() to allow variable number of args_w
                # (but this disables speed hacks in Pypy)
                args = Arguments(py_space, args_wp)
                self.set_result(W_PythonObject(
                    py_space.call_args(w_meth, args)))
            elif len(self.args_w) == 1:  # setattr when one argument
                wp_value = st_to_py(self.space(), self.args_w[0])
                py_space.setattr(wp_rcvr, wp_attrname, wp_value)
                self.set_result(W_PythonObject(py_space.w_None))
            else:  # otherwise getattr
                self.set_result(W_PythonObject(
                    py_space.getattr(wp_rcvr, wp_attrname)))
        except OperationError as operr:
            print 'Python error caught: %s' % operr
            error = utils.operr_to_w_object(operr)
            self.set_error(error)
            self.set_result(error)
        except Exception as e:
            self.fail('Unable to call %s on %s: %s' % (
                self.method_name, wp_rcvr, e))

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

    def guess_classname(self):
        return self.repr_classname

    def str_content(self):
        return '%s: "%s"' % (self.cmd, self.source)
