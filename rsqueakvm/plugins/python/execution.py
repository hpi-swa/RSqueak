from rsqueakvm.model.compiled_methods import (
    W_SpurCompiledMethod, W_PreSpurCompiledMethod)
from rsqueakvm.plugins.python import global_state as gs
from rsqueakvm.plugins.python.utils import _run_eval_string, operr_to_pylist

from rpython.rlib.rstacklet import StackletThread
from rpython.rlib import objectmodel

from pypy.interpreter.error import OperationError


def start_new_thread(source, filename, cmd, translated):
    # import pdb; pdb.set_trace()
    if translated:
        cls = StackletLanguageRunner
    else:
        cls = GreenletLanguageRunner
    language = PythonLanguage(source, filename, cmd)
    runner = cls(language)
    gs.py_runner.set(runner)
    runner.start()


def resume_thread():
    runner = gs.py_runner.get()
    if runner is None or not runner.resumable():
        print 'No runner to resume with'
        return False
    runner.resume()
    return gs.wp_operror.get() is None


class ForeignLanguage:
    pass


class PythonLanguage(ForeignLanguage):
    def __init__(self, source, filename, cmd):
        self.source = source
        self.filename = filename
        self.cmd = cmd
        self._resumable = True

    def run(self):
        print 'Python start'
        try:
            # ensure py_space has a fresh exectioncontext
            gs.py_space.threadlocals.enter_thread(gs.py_space)

            # switch back to Squeak before executing Python code
            gs.switch_action.perform()

            retval = _run_eval_string(self.source, self.filename, self.cmd)
            self.save_result(retval)
        except OperationError as operr:
            # operr was not handled by users, because they pressed proceed.
            # save Python error as result instead.
            self.save_result(operr_to_pylist(operr))
        except Exception as e:
            print 'Unknown error in Python thread: %s' % e
        finally:
            self._resumable = False

    def save_result(self, result):
        gs.wp_result.set(result)
        gs.wp_operror.set(None)  # unset last error


class GlobalState:
    def clear(self):
        self.origin = None
global_execution_state = GlobalState()
global_execution_state.clear()


class AbstractLanguageRunner:
    def __init__(self, language):
        self.language = language

    def start(self):
        raise NotImplementedError

    def resume(self):
        raise NotImplementedError

    def resumable(self):
        return self.language._resumable

    def return_to_smalltalk(self):
        raise NotImplementedError


class StackletLanguageRunner(AbstractLanguageRunner):
    def __init__(self, language):
        AbstractLanguageRunner.__init__(self, language)
        self.sthread = None

    def start(self):
        self.sthread = StackletThread()
        global_execution_state.origin = self
        self.h1 = self.sthread.new(self.__class__.new_stacklet_callback)

    def resume(self):
        self.sthread.switch(self.h1)

    def return_to_smalltalk(self):
        self.sthread.switch(self.h2)

    @staticmethod
    def new_stacklet_callback(h, arg):
        print 'new_stacklet_callback:', h, arg
        self = global_execution_state.origin
        self.h2 = h
        global_execution_state.clear()
        self.language.run()
        global_execution_state.origin = self
        return self.h2


class GreenletLanguageRunner(AbstractLanguageRunner):
    def start(self):
        from greenlet import greenlet
        global_execution_state.origin = self
        self.greenlet = greenlet(self.__class__.new_greenlet_callback())
        self.resume()  # stacklets also start immediately

    def resume(self):
        self.greenlet.switch()

    def return_to_smalltalk(self):
        self.greenlet.parent.switch()

    @staticmethod
    def new_greenlet_callback():
        print 'new_greenlet_callback'
        self = global_execution_state.origin
        return self.language.run


def switch_to_smalltalk(interp, s_frame, first_call=False):
    from rsqueakvm.storage_contexts import ContextPartShadow

    # print 'Switch to Smalltalk'
    wp_result = gs.wp_result.get()
    if wp_result is not None:
        return _create_return_frame(interp.space, wp_result)

    resume_method = gs.w_python_resume_method.get()
    s_resume_frame = ContextPartShadow.build_method_context(
        interp.space,
        resume_method,
        gs.w_python_class.get()
    )
    # import pdb; pdb.set_trace()
    # we go one up, because the s_frame.w_method() is our fake method
    if first_call or s_frame.w_method() is not resume_method:
        # assert s_frame.w_method() is not resume_method
        s_resume_frame.store_s_sender(s_frame)
    else:
        assert s_frame.w_method() is resume_method
        s_resume_frame.store_s_sender(s_frame.s_sender())
    interp.quick_check_for_interrupt(s_resume_frame,
                                     dec=interp.interrupt_counter_size)
    # this will raise a ProcessSwitch if there are interrupts or timers ...
    return s_resume_frame


def _create_return_frame(space, wp_result):
    from rsqueakvm.storage_contexts import ContextPartShadow
    from rsqueakvm.plugins.python.utils import wrap
    print 'Python has finished and returned a result'
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
    w_cm.literals = [wrap(space, wp_result), space.w_nil,
                     w_cm.compiledin_class]
    gs.wp_result.set(None)
    return ContextPartShadow.build_method_context(
        space, w_cm, w_python_class)
