from pypy.interpreter.error import OperationError
from pypy.interpreter.executioncontext import PeriodicAsyncAction


class SwitchToSmalltalkAction(PeriodicAsyncAction):

    def __init__(self, py_space):
        PeriodicAsyncAction.__init__(self, py_space)

    def perform(self, ec, frame):
        from rsqueakvm.plugins.python import PythonPlugin

        # import pdb; pdb.set_trace()
        language = ec.current_language
        if language is None:
            return
        runner = language.runner()
        if runner is None:
            return

        # print 'Python yield'
        runner.return_to_smalltalk()
        # print 'Python continue'

        # operror has been in Smalltalk land, clear it now to allow resuming
        language.reset_error()

        # handle py_frame_restart_info if set
        restart_info = PythonPlugin.py_frame_restart_info.get()
        if restart_info is not None:
            PythonPlugin.py_frame_restart_info.set(None)
            # import pdb; pdb.set_trace()
            raise RestartException(restart_info)


class RestartException(OperationError):
    def __init__(self, py_frame_restart_info):
        self.py_frame_restart_info = py_frame_restart_info

    def _compute_value(self, space):
        print '_compute_value called in RestartException'
        return None


class PyFrameRestartInfo():
    def __init__(self, frame=None, code=None):
        self.frame = frame
        self.pycode = code
