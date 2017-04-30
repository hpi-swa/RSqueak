from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.foreign_language import ForeignLanguagePlugin
from rsqueakvm.util import system

try:
    from rsqueakvm.plugins.ruby import utils
    from rsqueakvm.plugins.ruby.model import W_RubyObject, RubyClassShadow
    from rsqueakvm.plugins.ruby.objspace import ruby_space
    from rsqueakvm.plugins.ruby.patching import patch_topaz
    from rsqueakvm.plugins.ruby.process import W_RubyProcess

    IMPORT_FAILED = False
except ImportError as e:
    try:
        __import__('topaz')
        # if topaz can be imported, then there must be a problem in the plugin
        import pdb
        pdb.set_trace()
    except Exception as e:
        print e
    IMPORT_FAILED = True


class RubyPlugin(ForeignLanguagePlugin):
    language_name = 'Ruby'

    def is_optional(self):
        return True

    def is_enabled(self):
        if IMPORT_FAILED:
            return False
        return ForeignLanguagePlugin.is_enabled(self)

    def is_operational(self):
        return (W_RubyProcess.w_foreign_class.get() is not None and
                RubyClassShadow.w_foreign_class.get() is not None)

    def setup(self):
        system.translationconfig.set(thread=True)
        system.translationconfig.set(continuation=True)
        patch_topaz()

    @staticmethod
    def startup(space, argv):
        ForeignLanguagePlugin.load_special_objects(
            space, RubyPlugin.language_name, W_RubyProcess, RubyClassShadow)
        ruby_space.setup(argv[0])

    @staticmethod
    def new_eval_process(space, args_w):
        if (len(args_w) != 3):
            raise PrimitiveFailedError
        source_w = args_w[0]
        filepath_w = args_w[1]
        if (not isinstance(source_w, W_BytesObject) or
                not isinstance(filepath_w, W_BytesObject)):
            raise PrimitiveFailedError
        source = space.unwrap_string(source_w)
        filepath = space.unwrap_string(filepath_w)
        break_on_exceptions = args_w[2] is space.w_true
        return W_RubyProcess(
            space, source=source, filepath=filepath,
            break_on_exceptions=break_on_exceptions)

    @staticmethod
    def new_send_process(space, w_rcvr, method_name, args_w):
        return W_RubyProcess(
            space, w_rcvr=w_rcvr, method_name=method_name, args_w=args_w,
            is_send=True)

    @staticmethod
    def w_object_class():
        return W_RubyObject

    @staticmethod
    def to_w_object(space, foreign_object):
        return utils.ruby_to_smalltalk(space, foreign_object.wr_object)

    @staticmethod
    def restart_specific_frame(space, args_w):
        print 'Not yet implemented for Topaz'
        return False
