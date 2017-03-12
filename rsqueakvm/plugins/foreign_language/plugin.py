from rsqueakvm.error import Exit, PrimitiveFailedError
from rsqueakvm.plugins.foreign_language import w_foreign_language_class
from rsqueakvm.plugins.foreign_language.language import W_ForeignLanguage
from rsqueakvm.plugins.plugin import Plugin


class ForeignLanguagePlugin(Plugin):
    def __init__(self):
        Plugin.__init__(self)
        self.register_default_primitives()

    # Abstract methods

    @staticmethod
    def w_language_class():
        raise NotImplementedError

    @staticmethod
    def w_object_class():
        raise NotImplementedError

    @staticmethod
    def to_w_object(foreign_object):
        raise NotImplementedError

    @staticmethod
    def startup(space, argv):
        foreign_language_class = space.smalltalk_at('ForeignLanguage')
        if foreign_language_class is None:
            # disable plugin?
            raise Exit('ForeignLanguage class not found.')
        w_foreign_language_class.set(foreign_language_class)

    # Default primitives

    def register_default_primitives(self):
        @self.expose_primitive(unwrap_spec=[object, str, str, str, bool],
                               result_is_new_frame=True)
        def eval(interp, s_frame, w_rcvr, source, filename, cmd,
                 break_on_exceptions):
            # import pdb; pdb.set_trace()
            language = self.w_language_class()(
                source, filename, cmd, break_on_exceptions)
            language.start()
            # when we are here, the foreign language process has yielded
            return language.switch_to_smalltalk(interp, s_frame,
                                                first_call=True)

        @self.expose_primitive(unwrap_spec=[object, object],
                               result_is_new_frame=True)
        def resume(interp, s_frame, w_rcvr, language):
            # print 'Smalltalk yield'
            # import pdb; pdb.set_trace()
            if not isinstance(language, W_ForeignLanguage):
                raise PrimitiveFailedError
            if not language.resume():
                raise PrimitiveFailedError
            return language.switch_to_smalltalk(interp, s_frame)

        @self.expose_primitive(unwrap_spec=[object, object])
        def lastError(interp, s_frame, w_rcvr, language):
            if not isinstance(language, W_ForeignLanguage):
                raise PrimitiveFailedError
            w_error = language.get_error()
            if w_error is None:
                print 'w_error was None in lastError'
                raise PrimitiveFailedError
            return w_error

        @self.expose_primitive(unwrap_spec=[object])
        def asSmalltalk(interp, s_frame, w_rcvr):
            if not isinstance(w_rcvr, self.w_object_class()):
                raise PrimitiveFailedError
            return self.to_w_object(interp.space, w_rcvr)
