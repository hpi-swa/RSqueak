from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.foreign_language.model import W_ForeignLanguageObject
from rsqueakvm.plugins.foreign_language.process import W_ForeignLanguageProcess
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.util.cells import Cell


class ForeignLanguagePlugin(Plugin):
    _attrs_ = ['_break_on_exceptions_during_sends']

    def __init__(self):
        Plugin.__init__(self)
        self.register_default_primitives()
        self._break_on_exceptions_during_sends = Cell(False)

    @staticmethod
    def load_special_objects(space, language_name, process_cls, shadow_cls):
        process_cls.load_special_objects(process_cls, language_name, space)
        shadow_cls.load_special_objects(shadow_cls, language_name, space)

    # Abstract methods

    def is_operational(self):
        raise NotImplementedError

    @staticmethod
    def new_eval_process(space, args_w):
        raise NotImplementedError

    @staticmethod
    def new_send_process(space, w_rcvr, method_name, args_w,
                         break_on_exceptions):
        raise NotImplementedError

    @staticmethod
    def w_object_class():
        raise NotImplementedError

    @staticmethod
    def to_w_object(foreign_object):
        raise NotImplementedError

    @staticmethod
    def restart_specific_frame(space, args_w):
        raise NotImplementedError

    # Default primitives

    def register_default_primitives(self):
        @self.expose_primitive(result_is_new_frame=True)
        def eval(interp, s_frame, argcount):
            if not self.is_operational():
                raise PrimitiveFailedError
            # import pdb; pdb.set_trace()
            args_w = s_frame.peek_n(argcount)
            language_process = self.new_eval_process(interp.space, args_w)
            # when we are here, the foreign language process has yielded
            frame = language_process.switch_to_smalltalk(
                interp, s_frame, first_call=True)
            s_frame.pop_n(argcount + 1)
            return frame

        @self.expose_primitive(unwrap_spec=[object],
                               result_is_new_frame=True)
        def resume(interp, s_frame, language_process):
            # print 'Smalltalk yield'
            # import pdb; pdb.set_trace()
            if not isinstance(language_process, W_ForeignLanguageProcess):
                raise PrimitiveFailedError
            language_process.resume()
            if language_process.error_detected():
                raise PrimitiveFailedError
            return language_process.switch_to_smalltalk(interp, s_frame)

        @self.expose_primitive(result_is_new_frame=True, compiled_method=True)
        def send(interp, s_frame, argcount, w_method):
            if not self.is_operational():
                raise PrimitiveFailedError
            # import pdb; pdb.set_trace()
            args_w = s_frame.peek_n(argcount)
            w_rcvr = s_frame.peek(argcount)
            w_selector_name = w_method.literalat0(interp.space, 2)
            if not isinstance(w_selector_name, W_BytesObject):
                raise PrimitiveFailedError
            method_name = interp.space.unwrap_string(w_selector_name)
            idx = method_name.find(':')
            if idx > 0:
                method_name = method_name[0:idx]
            language_process = self.new_send_process(
                interp.space, w_rcvr, method_name, args_w,
                self._break_on_exceptions_during_sends.get())
            frame = language_process.switch_to_smalltalk(
                interp, s_frame, first_call=True)
            s_frame.pop_n(argcount + 1)
            return frame

        @self.expose_primitive(unwrap_spec=[object])
        def lastError(interp, s_frame, language_process):
            if not isinstance(language_process, W_ForeignLanguageProcess):
                raise PrimitiveFailedError
            w_error = language_process.get_error()
            if w_error is None:
                print 'w_error was None in lastError'
                raise PrimitiveFailedError
            return w_error

        @self.expose_primitive(unwrap_spec=[object])
        def topFrame(interp, s_frame, language_process):
            if not isinstance(language_process, W_ForeignLanguageProcess):
                raise PrimitiveFailedError
            w_top_frame = language_process.w_top_frame()
            if w_top_frame is None:
                raise PrimitiveFailedError
            return w_top_frame

        @self.expose_primitive(unwrap_spec=[object])
        def asSmalltalk(interp, s_frame, w_rcvr):
            if not isinstance(w_rcvr, self.w_object_class()):
                raise PrimitiveFailedError
            return self.to_w_object(interp.space, w_rcvr)

        @self.expose_primitive()
        def restartSpecificFrame(interp, s_frame, argcount):
            # import pdb; pdb.set_trace()
            args_w = s_frame.peek_n(argcount)
            result = self.restart_specific_frame(interp.space, args_w)
            s_frame.pop_n(argcount + 1)
            return interp.space.wrap_bool(result)

        @self.expose_primitive(unwrap_spec=[object, object])
        def registerSpecificClass(interp, s_frame, w_rcvr, language_obj):
            if not isinstance(language_obj, W_ForeignLanguageObject):
                raise PrimitiveFailedError
            language_obj.class_shadow(interp.space).set_specific_class(w_rcvr)

        @self.expose_primitive(unwrap_spec=[object, bool])
        def setBreakOnExceptionsDuringSends(interp, s_frame, w_rcvr, value):
            self._break_on_exceptions_during_sends.set(value)
            return interp.space.wrap_bool(value)
