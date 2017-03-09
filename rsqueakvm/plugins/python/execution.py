from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.compiled_methods import (
    W_SpurCompiledMethod, W_PreSpurCompiledMethod)
from rsqueakvm.plugins.python import global_state as gs
from rsqueakvm.plugins.python.model import W_PythonObject
from rsqueakvm.plugins.python.utils import _run_eval_string, operr_to_pylist

from rpython.rlib import objectmodel, rstacklet

from pypy.interpreter.error import OperationError
from pypy.interpreter.executioncontext import ExecutionContext


ExecutionContext.current_language = None


class W_ForeignLanguage(W_AbstractObjectWithIdentityHash):
    _attrs_ = ['_runner', '_done']
    repr_classname = 'W_ForeignLanguage'

    def __init__(self):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self._done = False
        if gs.translating[0]:
            self._runner = StackletLanguageRunner(self)
        else:
            self._runner = GreenletLanguageRunner(self)

    def run(self):
        raise NotImplementedError

    def set_result(self, result):
        raise NotImplementedError

    def get_result(self):
        raise NotImplementedError

    def set_error(self, error):
        raise NotImplementedError

    def get_error(self):
        raise NotImplementedError

    def start(self):
        self.runner().start()

    def runner(self):
        return self._runner

    def resume(self):
        if self.is_done():
            # import pdb; pdb.set_trace()
            print 'The runner is done and cannot be resumed'
            return False
        self.runner().resume()
        return self.get_error() is None

    def is_done(self):
        return self._done

    def set_current(self):
        ec = gs.py_space.getexecutioncontext()
        ec.current_language = self

    def switch_to_smalltalk(self, interp, s_frame, first_call=False):
        from rsqueakvm.storage_contexts import ContextPartShadow

        # print 'Switch to Smalltalk'
        if self.is_done():
            return _create_return_frame(interp.space, self.get_result())

        resume_method = gs.w_python_resume_method.get()
        s_resume_frame = ContextPartShadow.build_method_context(
            interp.space,
            resume_method,
            gs.w_python_class.get(),
            [self]
        )
        # import pdb; pdb.set_trace()
        # we go one up, because the s_frame.w_method() is our fake method
        if first_call or s_frame.w_method() is not resume_method:
            # assert s_frame.w_method() is not resume_method
            s_resume_frame.store_s_sender(s_frame)
        else:
            if s_frame.w_method() is not resume_method:
                print 'Unexpected s_frame found.'
            s_resume_frame.store_s_sender(s_frame.s_sender())
        interp.quick_check_for_interrupt(s_resume_frame,
                                         dec=interp.interrupt_counter_size)
        # this will raise a ProcessSwitch if there are interrupts or timers ...
        return s_resume_frame

    # Overrides

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
        return gs.w_foreign_language_class.get()


class W_PythonLanguage(W_ForeignLanguage):
    _attrs_ = ['source', 'filename', 'cmd', 'wp_result', 'wp_operror']
    repr_classname = 'W_PythonLanguage'

    def __init__(self, source, filename, cmd):
        W_ForeignLanguage.__init__(self)
        self.source = source
        self.filename = filename
        self.cmd = cmd
        self.wp_result = None
        self.wp_operror = None

    def run(self):
        print 'Python start'
        try:
            # ensure py_space has a fresh exectioncontext
            gs.py_space.threadlocals.enter_thread(gs.py_space)

            # switch back to Squeak before executing Python code
            self.runner().return_to_smalltalk()

            retval = _run_eval_string(self.source, self.filename, self.cmd)
            self.set_result(retval)
        except OperationError as operr:
            # operr was not handled by users, because they pressed proceed.
            # save Python error as result instead.
            self.set_result(operr_to_pylist(operr))
        except Exception as e:
            print 'Unknown error in Python thread: %s' % e
        finally:
            self._done = True

    def set_result(self, result):
        self.wp_result = result

    def get_result(self):
        return self.wp_result

    def set_error(self, error):
        self.wp_operror = error

    def get_error(self):
        return self.wp_operror


class GlobalState:
    def clear(self):
        self.origin = None
global_execution_state = GlobalState()
global_execution_state.clear()


class AbstractLanguageRunner():

    def __init__(self, language):
        self._language = language

    def language(self):
        return self._language

    def start(self):
        self.language().set_current()
        self.start_thread()

    def start_thread(self):
        raise NotImplementedError

    def resume(self):
        self.language().set_current()
        self.resume_thread()

    def resume_thread(self):
        raise NotImplementedError

    def return_to_smalltalk(self):
        raise NotImplementedError


class StackletLanguageRunner(AbstractLanguageRunner):
    sthread = None

    def __init__(self, language):
        AbstractLanguageRunner.__init__(self, language)
        self.sthread = self.ensure_sthread()
        self.language_handle = self.sthread.get_null_handle()
        self.smalltalk_handle = self.sthread.get_null_handle()

    def ensure_sthread(self):
        if self.sthread is None:
            self.sthread = rstacklet.StackletThread()
        return self.sthread

    def start_thread(self):
        global_execution_state.origin = self
        self.language_handle = self.sthread.new(
            self.__class__.new_stacklet_callback)

    def resume_thread(self):
        if not self._is_valid_handle(self.language_handle):
            print 'language_handle not valid'
            return
        self.sthread.switch(self.language_handle)

    def return_to_smalltalk(self):
        if not self._is_valid_handle(self.smalltalk_handle):
            print 'smalltalk_handle not valid'
            return
        self.sthread.switch(self.smalltalk_handle)

    def _is_valid_handle(self, h):
        if (self.sthread.is_empty_handle(h) or
                h == self.sthread.get_null_handle()):
            return False
        return True

    @staticmethod
    def new_stacklet_callback(h, arg):
        print 'new_stacklet_callback:', h, arg
        self = global_execution_state.origin
        self.smalltalk_handle = h
        global_execution_state.clear()
        self.language().run()
        global_execution_state.origin = self
        return self.smalltalk_handle  # return to Smalltalk when done


class GreenletLanguageRunner(AbstractLanguageRunner):
    def start_thread(self):
        from greenlet import greenlet
        global_execution_state.origin = self
        self.greenlet = greenlet(self.__class__.new_greenlet_callback())
        self.resume()  # stacklets also start immediately

    def resume_thread(self):
        self.greenlet.switch()

    def return_to_smalltalk(self):
        self.greenlet.parent.switch()

    @staticmethod
    def new_greenlet_callback():
        print 'new_greenlet_callback'
        self = global_execution_state.origin
        return self.language().run


def _create_return_frame(space, wp_result):
    from rsqueakvm.storage_contexts import ContextPartShadow
    print 'Python has finished and returned a result.'
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
    w_python_class = gs.w_python_class.get()
    w_cm.header = 0
    w_cm._primitive = 0
    w_cm.literalsize = 3
    w_cm.islarge = False
    w_cm._tempsize = 0
    w_cm.argsize = 0
    w_cm.compiledin_class = w_python_class.getclass(space)
    w_cm.lookup_selector = 'fakeReturnResult'
    w_cm.bytes = [chr(b) for b in [
        0x20,  # push constant
        0x7C,  # return stack top
    ]]
    w_cm.literals = [W_PythonObject(wp_result), space.w_nil,
                     w_cm.compiledin_class]
    return ContextPartShadow.build_method_context(
        space, w_cm, w_python_class)
