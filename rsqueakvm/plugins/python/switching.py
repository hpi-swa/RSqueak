from pypy.interpreter.error import OperationError
from pypy.interpreter.executioncontext import PeriodicAsyncAction


class SwitchToSmalltalkAction(PeriodicAsyncAction):

    def __init__(self, py_space):
        PeriodicAsyncAction.__init__(self, py_space)

    def perform(self, ec, frame):
        from rsqueakvm.plugins.python import PythonPlugin

        # import pdb; pdb.set_trace()
        process = self.space.current_python_process.get()
        if process is None:
            print 'no language process'
            return
        runner = process.runner()
        if runner is None:
            print 'no runner'
            return
        if not runner.resumable():
            print 'not resumable'
            return

        # print 'Python yield'
        runner.return_to_smalltalk()
        # print 'Python continue'

        # operror has been in Smalltalk land, clear it now to allow resuming
        process.reset_error()

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
