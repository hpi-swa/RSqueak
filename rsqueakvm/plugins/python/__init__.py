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

    from pypy.interpreter.argument import Arguments
    from pypy.interpreter.error import OperationError

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
        return (W_PythonProcess.w_foreign_class.get() is not None and
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
    def new_language_process(space, args_w):
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
        return W_PythonProcess(source, filename, cmd, break_on_exceptions)

    @staticmethod
    def w_object_class():
        return W_PythonObject

    @staticmethod
    def perform_send(space, w_rcvr, attrname, args_w):
        wp_rcvr = utils.smalltalk_to_python(space, w_rcvr)
        idx = attrname.find(':')
        if idx > 0:
            attrname = attrname[0:idx]
        wp_attrname = py_space.newtext(attrname)
        # import pdb; pdb.set_trace()
        try:
            if attrname == '__call__':  # special __call__ case
                w_meth = py_space.getattr(wp_rcvr, wp_attrname)
                args_wp = [utils.smalltalk_to_python(space, a) for a in args_w]
                # use call_args() to allow variable number of args_w
                # (but this disables speed hacks in Pypy)
                args = Arguments(py_space, args_wp)
                return W_PythonObject(py_space.call_args(w_meth, args))
            elif len(args_w) == 1:  # setattr when one argument
                wp_value = utils.smalltalk_to_python(space, args_w[0])
                py_space.setattr(wp_rcvr, wp_attrname, wp_value)
                return W_PythonObject(py_space.w_None)
            else:  # otherwise getattr
                return W_PythonObject(py_space.getattr(wp_rcvr, wp_attrname))
        except OperationError as operr:
            return utils.operr_to_w_object(operr)
        except Exception as e:
            print 'Unable to call %s on %s: %s' % (attrname, wp_rcvr, e)
        raise PrimitiveFailedError

    @staticmethod
    def to_w_object(space, foreign_object):
        return utils.python_to_smalltalk(space, foreign_object.wp_object)

    @staticmethod
    def set_py_frame_restart_info(frame, py_code):
        PythonPlugin.py_frame_restart_info.set(
            PyFrameRestartInfo(frame, py_code))
