from rsqueakvm.error import Exit
from rsqueakvm.plugins import plugin
from rsqueakvm.plugins.foreign_language.plugin import ForeignLanguagePlugin
from rsqueakvm.util import system

from rsqueakvm.plugins import python
from rsqueakvm.plugins.python import model, utils
from rsqueakvm.plugins.python.language import W_PythonLanguage
from rsqueakvm.plugins.python.patching import patch_pypy
from rsqueakvm.plugins.python.switching import PyFrameRestartInfo


class PythonPlugin(ForeignLanguagePlugin):
    def is_optional(self):
        return True

    def setup(self):
        system.translationconfig.set(thread=True)
        system.translationconfig.set(continuation=True)
        patch_pypy()

    @staticmethod
    def startup(space, argv):
        ForeignLanguagePlugin.startup(space, argv)

        python.w_python_plugin_send.set(space.wrap_list_unroll_safe([
            space.wrap_string('PythonPlugin'),
            space.wrap_string('send')
        ]))

        python_class = space.smalltalk_at('Python')
        if python_class is None:
            # disable plugin?
            raise Exit('Python class not found.')
        python.w_python_class.set(python_class)

        python_object_class = space.smalltalk_at('PythonObject')
        if python_object_class is None:
            raise Exit('PythonObject class not found.')
        python.w_python_object_class.set(python_object_class)

        resume_method_symbol = space.wrap_symbol('resume:')
        python_cls_cls_s = python_class.getclass(
            space).as_class_get_shadow(space)
        resume_method = python_cls_cls_s.lookup(resume_method_symbol)
        if resume_method is None:
            raise Exit('Python class>>resumeFrame method not found.')
        python.w_python_resume_method.set(resume_method)

        python.py_space.startup()

    @staticmethod
    def set_frame_restart_info(frame, py_code):
        plugin.py_frame_restart_info = PyFrameRestartInfo(frame, py_code)

    @staticmethod
    def w_language_class():
        return W_PythonLanguage

    @staticmethod
    def w_object_class():
        return model.W_PythonObject

    @staticmethod
    def to_w_object(space, foreign_object):
        return utils.python_to_smalltalk(space, foreign_object.wp_object)
