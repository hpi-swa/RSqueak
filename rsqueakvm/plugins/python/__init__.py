from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.foreign_language import ForeignLanguagePlugin
from rsqueakvm.util.cells import Cell
from rsqueakvm.util.system import translationconfig

try:
    from rsqueakvm.plugins.python.model import (
        W_PythonObject, PythonClassShadow)
    from rsqueakvm.plugins.python.language import W_PythonLanguage
    from rsqueakvm.plugins.python.objspace import py_space
    from rsqueakvm.plugins.python.patching import patch_pypy
    from rsqueakvm.plugins.python.switching import PyFrameRestartInfo
    from rsqueakvm.plugins.python.utils import python_to_smalltalk
    IMPORT_FAILED = False
except ImportError:
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

    def setup(self):
        translationconfig.set(thread=True)
        translationconfig.set(continuation=True)
        patch_pypy()

    @staticmethod
    def startup(space, argv):
        ForeignLanguagePlugin.load_special_objects(
            space, PythonPlugin.language_name,
            W_PythonLanguage, PythonClassShadow)
        py_space.startup()

    @staticmethod
    def new_w_language(space, args_w):
        if len(args_w) != 4:
            raise PrimitiveFailedError
        source = space.unwrap_string(args_w[0])
        filename = space.unwrap_string(args_w[1])
        cmd = space.unwrap_string(args_w[2])
        break_on_exceptions = args_w[3] is space.w_true
        return W_PythonLanguage(source, filename, cmd, break_on_exceptions)

    @staticmethod
    def w_object_class():
        return W_PythonObject

    @staticmethod
    def to_w_object(space, foreign_object):
        return python_to_smalltalk(space, foreign_object.wp_object)

    @staticmethod
    def set_py_frame_restart_info(frame, py_code):
        PythonPlugin.py_frame_restart_info.set(
            PyFrameRestartInfo(frame, py_code))
