from rsqueakvm.plugins.foreign_language import runner, w_foreign_language_class
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.compiled_methods import (
    W_PreSpurCompiledMethod, W_SpurCompiledMethod)

from pypy.interpreter.executioncontext import ExecutionContext

from rpython.rlib import objectmodel


ExecutionContext.current_language = None


class W_ForeignLanguage(W_AbstractObjectWithIdentityHash):
    _attrs_ = [
        '_runner', '_done', 'w_result', 'w_error', '_break_on_exceptions']
    repr_classname = 'W_ForeignLanguage'

    def __init__(self, break_on_exceptions=True):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self._done = False
        self.w_result = None
        self.w_error = None
        self._break_on_exceptions = break_on_exceptions
        if objectmodel.we_are_translated():
            self._runner = runner.StackletLanguageRunner(self)
        else:
            self._runner = runner.GreenletLanguageRunner(self)

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
        return w_foreign_language_class.get()

    # Abstract methods

    def run(self):
        raise NotImplementedError

    def set_result(self, result):
        raise NotImplementedError

    def set_error(self, error):
        raise NotImplementedError

    def set_current(self):
        raise NotImplementedError

    def resume_class(self):
        raise NotImplementedError

    def resume_method(self):
        raise NotImplementedError

    # Helpers

    def start(self):
        self.runner().start()

    def resume(self):
        if self.is_done():
            # import pdb; pdb.set_trace()
            print 'The runner is done and cannot be resumed'
            return False
        self.runner().resume()
        return self.get_error() is None

    def runner(self):
        return self._runner

    def mark_done(self):
        self._done = True

    def is_done(self):
        return self._done

    def get_result(self):
        return self.w_result

    def get_error(self):
        return self.w_error

    def reset_error(self):
        self.w_error = None

    def break_on_exceptions(self):
        return self._break_on_exceptions

    # Switching

    def switch_to_smalltalk(self, interp, s_frame, first_call=False):
        from rsqueakvm.storage_contexts import ContextPartShadow

        # print 'Switch to Smalltalk'
        if self.is_done():
            return self._create_return_frame(interp.space)

        s_resume_frame = ContextPartShadow.build_method_context(
            interp.space,
            self.resume_method(),
            self.resume_class(),
            [self]
        )
        # import pdb; pdb.set_trace()
        # we go one up, because the s_frame.w_method() is our fake method
        if first_call or s_frame.w_method() is not self.resume_method():
            # assert s_frame.w_method() is not resume_method
            s_resume_frame.store_s_sender(s_frame)
        else:
            if s_frame.w_method() is not self.resume_method():
                print 'Unexpected s_frame found.'
            s_resume_frame.store_s_sender(s_frame.s_sender())
        interp.quick_check_for_interrupt(s_resume_frame,
                                         dec=interp.interrupt_counter_size)
        # this will raise a ProcessSwitch if there are interrupts or timers ...
        return s_resume_frame

    def _create_return_frame(self, space):
        from rsqueakvm.storage_contexts import ContextPartShadow
        print 'Language has finished and returned a result.'
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
        w_resume_class = self.resume_class()
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
