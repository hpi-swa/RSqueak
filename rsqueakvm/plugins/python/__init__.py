from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.foreign_language import ForeignLanguagePlugin
from rsqueakvm.util.cells import Cell
from rsqueakvm.util.system import translationconfig

try:
    from rsqueakvm.plugins.python import utils
    from rsqueakvm.plugins.python.model import (
        W_PythonObject, PythonClassShadow)
    from rsqueakvm.plugins.python.language import W_PythonLanguage
    from rsqueakvm.plugins.python.objspace import py_space
    from rsqueakvm.plugins.python.patching import patch_pypy
    from rsqueakvm.plugins.python.switching import PyFrameRestartInfo

    from pypy.interpreter.argument import Arguments
    from pypy.interpreter.error import OperationError
    from pypy.interpreter.function import (
        Function, Method, StaticMethod, ClassMethod)

    IMPORT_FAILED = False
except ImportError:
    try:
        __import__('pypy')
        # if pypy can be imported, then there must be a problem in the plugin
        import pdb
        pdb.set_trace()
    except:
        pass
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
        return (W_PythonLanguage.w_foreign_class.get() is not None and
                PythonClassShadow.w_foreign_class.get() is not None)

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
        return W_PythonLanguage(source, filename, cmd, break_on_exceptions)

    @staticmethod
    def w_object_class():
        return W_PythonObject

    @staticmethod
    def perform_send(space, w_rcvr, attrname, args_w):
        wp_rcvr = utils.smalltalk_to_python(space, w_rcvr)
        idx = attrname.find(':')
        if idx > 0:
            attrname = attrname[0:idx]
        wp_result = None
        try:
            py_attr = py_space.getattr(wp_rcvr, py_space.newtext(attrname))
            if (isinstance(py_attr, Function) or
                    isinstance(py_attr, Method) or
                    isinstance(py_attr, StaticMethod) or
                    isinstance(py_attr, ClassMethod)):
                args_wp = [utils.smalltalk_to_python(space, a) for a in args_w]
                # use call_args() to allow variable number of args_w
                # (but this disables speed hacks in Pypy)
                w_meth = py_space.getattr(wp_rcvr, py_space.newtext(attrname))
                args = Arguments(py_space, args_wp)
                wp_result = py_space.call_args(w_meth, args)
            else:
                if len(args_w) == 1:
                    wp_value = utils.smalltalk_to_python(space, args_w[0])
                    py_space.setattr(wp_rcvr, py_space.newtext(attrname),
                                     wp_value)
                    wp_result = py_space.w_None
                else:
                    wp_result = py_attr
        except OperationError as operr:
            return W_PythonObject(utils.operr_to_pylist(operr))
        except Exception as e:
            print 'Unable to call %s on %s: %s' % (attrname, wp_rcvr, e)
            raise PrimitiveFailedError
        if wp_result is None:
            # import pdb; pdb.set_trace()
            print ('No result in send primitive (wp_rcvr: %s, attrname: %s)'
                   % (wp_rcvr, attrname))
            raise PrimitiveFailedError
        return W_PythonObject(wp_result)

    @staticmethod
    def to_w_object(space, foreign_object):
        return utils.python_to_smalltalk(space, foreign_object.wp_object)

    @staticmethod
    def set_py_frame_restart_info(frame, py_code):
        PythonPlugin.py_frame_restart_info.set(
            PyFrameRestartInfo(frame, py_code))
