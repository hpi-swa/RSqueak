from rsqueakvm.plugins.foreign_language import runner
from rsqueakvm.plugins.foreign_language.utils import log
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.compiled_methods import (
    W_PreSpurCompiledMethod, W_SpurCompiledMethod)
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.util.cells import QuasiConstant

from rpython.rlib import objectmodel


class ForeignLanguageProcessMeta(type):
    def __new__(cls, name, bases, attrs):
        # import pdb; pdb.set_trace()

        if name != 'W_ForeignLanguageProcess':
            w_foreign_process_class = QuasiConstant(None, cls=W_PointersObject)

            def foreign_process_class(self):
                return w_foreign_process_class.get()

            attrs['w_foreign_process_class'] = w_foreign_process_class
            attrs['foreign_process_class'] = foreign_process_class

        return type.__new__(cls, name, bases, attrs)


class W_ForeignLanguageProcess(W_AbstractObjectWithIdentityHash):
    __metaclass__ = ForeignLanguageProcessMeta
    _attrs_ = [
        '_runner', '_done', 'w_result', 'w_error',
        '_space', 'w_rcvr', 'method_name', 'args_w',
        '_is_send', '_break_on_exceptions']
    repr_classname = 'W_ForeignLanguageProcess'

    eval_method = QuasiConstant(None, cls=W_PointersObject)
    resume_method = QuasiConstant(None, cls=W_PointersObject)

    def __init__(self, space, w_rcvr=None, method_name='', args_w=None,
                 is_send=False, break_on_exceptions=False):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self._space = space
        self.w_rcvr = w_rcvr
        self.method_name = method_name
        self.args_w = args_w or []
        self._done = False
        self.w_result = None
        self.w_error = None
        self._is_send = is_send
        self._break_on_exceptions = break_on_exceptions

    def init_runner(self):
        if objectmodel.we_are_translated():
            self._runner = runner.StackletLanguageRunner(self)
        else:
            self._runner = runner.GreenletLanguageRunner(self)

    def space(self):
        return self._space

    @staticmethod
    def load_special_objects(cls, language_name, space):
        language_process_class = space.smalltalk_at(
            '%sProcess' % language_name)
        if language_process_class is None:
            # disable plugin?
            print '%s class not found.' % language_name
        cls.w_foreign_process_class.set(language_process_class)

        # this part is called twice -> make better
        foreign_class = space.smalltalk_at('ForeignLanguageProcess')

        if foreign_class is None:
            print 'ForeignLanguage class not found.'
            return

        language_process_class_s = language_process_class.as_class_get_shadow(
            space)

        eval_method_symbol = space.wrap_symbol('vmEval')
        eval_method = language_process_class_s.lookup(eval_method_symbol)
        if eval_method is None:
            print 'ForeignLanguageProcess>>vmEval method not found.'
        W_ForeignLanguageProcess.eval_method.set(eval_method)

        resume_method_symbol = space.wrap_symbol('vmResume')
        resume_method = language_process_class_s.lookup(
            resume_method_symbol)
        if resume_method is None:
            print 'ForeignLanguageProcess>>vmResume: method not found.'
        W_ForeignLanguageProcess.resume_method.set(resume_method)

    # W_AbstractObjectWithIdentityHash overrides

    def at0(self, space, index0):
        # import pdb; pdb.set_trace()
        return space.w_nil

    def atput0(self, space, index0, w_value):
        # import pdb; pdb.set_trace()
        pass

    def fetch(self, space, n0):
        # import pdb; pdb.set_trace()
        return space.w_nil

    def store(self, space, n0, w_value):
        # import pdb; pdb.set_trace()
        pass

    def getclass(self, space):
        return self.foreign_process_class()

    # Abstract methods

    def eval(self):
        raise NotImplementedError

    def send(self):
        raise NotImplementedError

    def pre_resume(self):  # called on every switch to language process
        pass

    def post_resume(self):  # called as soon as Smalltalk continues
        pass

    def w_top_frame(self):
        raise NotImplementedError

    # Helpers

    def safe_run(self):
        try:
            if self._is_send:
                self.guarded_send()
            else:
                self.eval()
        except Exception as e:
            print 'Unknown error in thread: %s' % e
        finally:
            self._done = True

    def guarded_send(self):
        if (self.w_rcvr is None or self.method_name == ''):
            error_msg = 'Invalid send (w_rcvr: %s, method: %s, args_w: %s)' % (
                self.w_rcvr, self.method_name, self.args_w)
            print error_msg
            self.set_error(self.space().wrap_string(error_msg))
            return
        self.send()

    def resume(self):
        if self.is_done():
            # import pdb; pdb.set_trace()
            print 'The runner is done and cannot be resumed'
            return
        self.runner().resume()

    def error_detected(self):
        if not self.break_on_exceptions():
            return False  # pretend everything is ok and move on
        return self.get_error() is not None

    def runner(self):
        return self._runner

    def is_done(self):
        return self._done

    def fail(self, error_msg):
        print error_msg
        self.set_error(self.space().wrap_string(error_msg))

    def get_result(self):
        return self.w_result

    def set_result(self, w_result):
        self.w_result = w_result

    def get_error(self):
        return self.w_error

    def set_error(self, w_error):
        self.w_error = w_error

    def reset_error(self):
        self.w_error = None

    def break_on_exceptions(self):
        return self._break_on_exceptions

    # Switching

    def switch_to_smalltalk(self, interp, s_frame, first_call=False):
        self.post_resume()
        log('Switching to Smalltalk...')

        if self.is_done():
            return self._create_return_frame(interp.space)

        # import pdb; pdb.set_trace()
        if first_call:  # attach s_frame with resume method for the first time
            s_resume_frame = self._build_resume_method(
                interp.space, is_eval=True)
            s_resume_frame.store_s_sender(s_frame)
        elif s_frame.w_method() is self.resume_method.get():
            s_resume_frame = self._build_resume_method(interp.space)
            eval_frame = s_frame.s_sender()
            # Ensure #resume: method with closure = nil
            if (eval_frame.w_method() is self.eval_method.get() and
                    eval_frame.closure is not None):
                # instead of chaining resume frames, store original sender
                s_resume_frame.store_s_sender(eval_frame)
            else:
                print ('Unexpected eval_frame found:\n%s' %
                       s_frame.print_stack())
                s_resume_frame.store_s_sender(s_frame)
        else:
            print 'Unexpected s_frame found:\n%s' % s_frame.print_stack()
            s_resume_frame = s_frame
        interp.quick_check_for_interrupt(s_resume_frame,
                                         dec=interp.interrupt_counter_size)
        # this will raise a ProcessSwitch if there are interrupts or timers ...
        return s_resume_frame

    def _build_resume_method(self, space, is_eval=False):
        from rsqueakvm.storage_contexts import ContextPartShadow
        if is_eval:
            method = self.eval_method.get()
        else:
            method = self.resume_method.get()
        return ContextPartShadow.build_method_context(space, method, self)

    def _create_return_frame(self, space):
        from rsqueakvm.storage_contexts import ContextPartShadow
        log('Language has finished and returned a result.')
        # we want evalInThread and resumePython to return new frames,
        # so we don't build up stack, but we also don't raise to the
        # top-level loop all the time.
        # For resuming, we obviously need a new frame, because that's
        # how the Smalltalk scheduler knows how to continue back to Python.
        # Unfortunately, a primitive can only EITHER always return a new
        # frame OR a result. So when we get a result, we cannot simply
        # return it. Instead, we need to build a frame that simply returns
        # the result
        if space.is_spur.is_set():
            w_cm = objectmodel.instantiate(W_SpurCompiledMethod)
        else:
            w_cm = objectmodel.instantiate(W_PreSpurCompiledMethod)
        w_resume_class = self.foreign_process_class()
        w_cm.header = 0
        w_cm._primitive = 0
        w_cm.literalsize = 3
        w_cm.islarge = False
        w_cm._tempsize = 0
        w_cm.argsize = 0
        w_cm.compiledin_class = w_resume_class.getclass(space)
        w_cm.lookup_selector = 'fakeReturnResult'
        w_cm.bytes = [chr(b) for b in [
            0x20,  # push constant
            0x7C,  # return stack top
        ]]
        w_cm.literals = [self.get_result(), space.w_nil, w_cm.compiledin_class]
        return ContextPartShadow.build_method_context(
            space, w_cm, w_resume_class)
