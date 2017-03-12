from rsqueakvm.error import Exit, PrimitiveFailedError
from rsqueakvm.plugins.foreign_language.plugin import ForeignLanguagePlugin
from rsqueakvm.util import system

try:
    from rsqueakvm.plugins import ruby
    from rsqueakvm.plugins.ruby.model import W_RubyObject
    from rsqueakvm.plugins.ruby.language import W_RubyLanguage
    from rsqueakvm.plugins.ruby.patching import patch_topaz
    from rsqueakvm.plugins.ruby.utils import ruby_to_smalltalk
except ImportError:
    pass


class RubyPlugin(ForeignLanguagePlugin):
    def is_optional(self):
        return True

    def setup(self):
        system.translationconfig.set(thread=True)
        system.translationconfig.set(continuation=True)
        patch_topaz()

    @staticmethod
    def startup(space, argv):
        ForeignLanguagePlugin.startup(space, argv)

        ruby.w_ruby_plugin_send.set(space.wrap_list_unroll_safe([
            space.wrap_string('RubyPlugin'),
            space.wrap_string('send')
        ]))

        ruby_class = space.smalltalk_at('Ruby')
        if ruby_class is None:
            # disable plugin?
            raise Exit('Ruby class not found.')
        ruby.w_ruby_class.set(ruby_class)

        ruby_object_class = space.smalltalk_at('RubyObject')
        if ruby_object_class is None:
            raise Exit('RubyObject class not found.')
        ruby.w_ruby_object_class.set(ruby_object_class)

        resume_method_symbol = space.wrap_symbol('resume:')
        ruby_cls_cls_s = ruby_class.getclass(
            space).as_class_get_shadow(space)
        resume_method = ruby_cls_cls_s.lookup(resume_method_symbol)
        if resume_method is None:
            raise Exit('Ruby class>>resume: method not found.')
        ruby.w_ruby_resume_method.set(resume_method)

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
