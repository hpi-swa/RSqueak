from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins.foreign_language import ForeignLanguagePlugin
from rsqueakvm.util import system

try:
    from rsqueakvm.plugins.ruby.model import W_RubyObject, RubyClassShadow
    from rsqueakvm.plugins.ruby.language import W_RubyLanguage
    from rsqueakvm.plugins.ruby.patching import patch_topaz
    from rsqueakvm.plugins.ruby.utils import ruby_to_smalltalk
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
    def to_w_object(space, foreign_object):
        return ruby_to_smalltalk(space, foreign_object.wr_object)
