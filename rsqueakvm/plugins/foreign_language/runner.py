from rpython.rlib import rstacklet


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
        self.language_handle = self.sthread.switch(self.language_handle)

    def return_to_smalltalk(self):
        if not self._is_valid_handle(self.smalltalk_handle):
            print 'smalltalk_handle not valid'
            return
        self.smalltalk_handle = self.sthread.switch(self.smalltalk_handle)

    def _is_valid_handle(self, h):
        if (h is None or self.sthread.is_empty_handle(h) or
                h == self.sthread.get_null_handle()):
            return False
        return True

    @staticmethod
    def new_stacklet_callback(h, arg):
        print 'new_stacklet_callback:', h, arg
        self = global_execution_state.origin
        self.smalltalk_handle = h
        global_execution_state.clear()
        self.language().run_safe()
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
        return self.language().run_safe


class GlobalState:
    def clear(self):
        self.origin = None
global_execution_state = GlobalState()
global_execution_state.clear()
