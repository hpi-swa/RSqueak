from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.foreign_language import ForeignLanguagePlugin
from rsqueakvm.util.cells import Cell
from rsqueakvm.util.system import translationconfig

try:
    from rsqueakvm.plugins.python import utils
    from rsqueakvm.plugins.python.model import (
        W_PythonObject, PythonClassShadow)
    from rsqueakvm.plugins.python.objspace import initialize_py_space, py_space
    from rsqueakvm.plugins.python.patching import patch_pypy
    from rsqueakvm.plugins.python.process import W_PythonProcess
    from rsqueakvm.plugins.python.switching import PyFrameRestartInfo

    IMPORT_FAILED = False
except ImportError:
    try:
        __import__('pypy')
        # if pypy can be imported, then there must be a problem in the plugin
        import pdb
        pdb.set_trace()
    except Exception as e:
        print e
    IMPORT_FAILED = True


class PythonPlugin(ForeignLanguagePlugin):
    language_name = 'Python'

    py_frame_restart_info = Cell(None, type=PyFrameRestartInfo)

    def is_optional(self):
        return True

    def is_enabled(self):
        if IMPORT_FAILED:
            return False
        return ForeignLanguagePlugin.is_enabled(self)

    def is_operational(self):
        return (W_PythonProcess.w_foreign_process_class.get() is not None and
                PythonClassShadow.w_foreign_class.get() is not None)

    def setup(self):
        translationconfig.set(thread=True)
        translationconfig.set(continuation=True)
        patch_pypy()

    @staticmethod
    def startup(space, argv):
        ForeignLanguagePlugin.load_special_objects(
            space, PythonPlugin.language_name,
            W_PythonProcess, PythonClassShadow)
        initialize_py_space(space, argv)
        py_space.startup()

    @staticmethod
    def new_eval_process(space, args_w):
        if len(args_w) != 4:
            raise PrimitiveFailedError
        source_w = args_w[0]
        filename_w = args_w[1]
        cmd_w = args_w[2]
        if (not isinstance(source_w, W_BytesObject) or
                not isinstance(filename_w, W_BytesObject) or
                not isinstance(cmd_w, W_BytesObject)):
            raise PrimitiveFailedError
        source = space.unwrap_string(source_w)
        filename = space.unwrap_string(filename_w)
        cmd = space.unwrap_string(cmd_w)
        break_on_exceptions = args_w[3] is space.w_true
        return W_PythonProcess(
            space, source=source, filename=filename, cmd=cmd,
            break_on_exceptions=break_on_exceptions)

    @staticmethod
    def new_send_process(space, w_rcvr, method_name, args_w):
        return W_PythonProcess(
            space, is_send=True,
            w_rcvr=w_rcvr, method_name=method_name, args_w=args_w)

    @staticmethod
    def w_object_class():
        return W_PythonObject

    @staticmethod
    def to_w_object(space, foreign_object):
        return utils.python_to_smalltalk(space, foreign_object.wp_object)

    @staticmethod
    def set_py_frame_restart_info(frame, py_code):
        PythonPlugin.py_frame_restart_info.set(
            PyFrameRestartInfo(frame, py_code))

    @staticmethod
    def restart_specific_frame(space, args_w):
        frame_w = args_w[0]
        source_w = args_w[1]
        filename_w = args_w[2]
        cmd_w = args_w[3]

        if not (isinstance(frame_w, W_PythonObject) and
                isinstance(source_w, W_BytesObject) and
                isinstance(filename_w, W_BytesObject) and
                isinstance(cmd_w, W_BytesObject)):
            return False
        frame = frame_w.wp_object
        source = space.unwrap_string(source_w)
        filename = space.unwrap_string(filename_w)
        cmd = space.unwrap_string(cmd_w)

        py_code = None
        if source:
            py_code = utils.get_restart_pycode(source, filename, cmd)
            if py_code is None:
                return False
        PythonPlugin.set_py_frame_restart_info(frame, py_code)
        return True
