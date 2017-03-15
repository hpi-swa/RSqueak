from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.foreign_language import ForeignLanguagePlugin
from rsqueakvm.util import system

try:
    from rsqueakvm.plugins.ruby import utils
    from rsqueakvm.plugins.ruby.frame import WR_FrameObject
    from rsqueakvm.plugins.ruby.language import W_RubyLanguage
    from rsqueakvm.plugins.ruby.model import W_RubyObject, RubyClassShadow
    from rsqueakvm.plugins.ruby.objspace import ruby_space
    from rsqueakvm.plugins.ruby.patching import patch_topaz

    from topaz.error import RubyError, print_traceback

    IMPORT_FAILED = False
except ImportError as e:
    IMPORT_FAILED = True


class RubyPlugin(ForeignLanguagePlugin):
    language_name = 'Ruby'

    def is_optional(self):
        return True

    def is_enabled(self):
        if IMPORT_FAILED:
            return False
        return ForeignLanguagePlugin.is_enabled(self)

    def setup(self):
        system.translationconfig.set(thread=True)
        system.translationconfig.set(continuation=True)
        patch_topaz()

    @staticmethod
    def startup(space, argv):
        ForeignLanguagePlugin.load_special_objects(
            space, RubyPlugin.language_name,
            W_RubyLanguage, RubyClassShadow)
        ruby_space.setup(argv[0])

    @staticmethod
    def new_w_language(space, args_w):
        if len(args_w) != 2:
            raise PrimitiveFailedError
        source = space.unwrap_string(args_w[0])
        break_on_exceptions = args_w[1] is space.w_true
        return W_RubyLanguage(source, break_on_exceptions)

    @staticmethod
    def w_object_class():
        return W_RubyObject

    @staticmethod
    def perform_send(space, w_rcvr, methodname, args_w):
        wr_rcvr = utils.smalltalk_to_ruby(space, w_rcvr)
        args_rw = [utils.smalltalk_to_ruby(space, w_arg) for w_arg in args_w]
        idx = methodname.find(':')
        if idx > 0:
            methodname = methodname[0:idx]
        wr_result = None
        try:
            wr_result = ruby_space.send(wr_rcvr, methodname, args_w=args_rw)
        except RubyError as e:
            print_traceback(ruby_space, e.w_value)
            raise PrimitiveFailedError
        if wr_result is None:
            # import pdb; pdb.set_trace()
            print ('No result in send primitive (wr_rcvr: %s, methodname: %s)'
                   % (wr_rcvr, methodname))
            raise PrimitiveFailedError
        return W_RubyObject(wr_result)

    @staticmethod
    def top_w_frame():
        topframe = ruby_space.getexecutioncontext().gettoprubyframe()
        if topframe is None:
            raise PrimitiveFailedError
        return W_RubyObject(WR_FrameObject(topframe))

    @staticmethod
    def to_w_object(space, foreign_object):
        return utils.ruby_to_smalltalk(space, foreign_object.wr_object)
