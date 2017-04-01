from rpython.rlib import rstacklet


class AbstractLanguageRunner():

    def __init__(self, language_process):
        self._language_process = language_process

    def language_process(self):
        return self._language_process

    def start(self):
        self.language_process().pre_resume()
        self.start_thread()

    def resume(self):
        self.language_process().pre_resume()
        self.resume_thread()

    def return_to_smalltalk(self):
        self.yield_thread()

    def start_thread(self):
        raise NotImplementedError

    def resume_thread(self):
        raise NotImplementedError

    def resumable(self):
        raise NotImplementedError

    def yield_thread(self):
        raise NotImplementedError


class StackletLanguageRunner(AbstractLanguageRunner):
    sthread = None

    def __init__(self, language_process):
        AbstractLanguageRunner.__init__(self, language_process)
        self.sthread = rstacklet.StackletThread()
        # there can only be one valid handle at a time (main or foreign thread)
        self.handle = self.sthread.get_null_handle()

    def start_thread(self):
        global_execution_state.origin = self
        self.handle = self.sthread.new(self.__class__.new_stacklet_callback)

    def resume_thread(self):
        self.switch_to_handle()

    def resumable(self):
        return self._has_valid_handle()

    def yield_thread(self):
        self.switch_to_handle()

    def switch_to_handle(self):
        if not self._has_valid_handle():
            print 'handle not valid: %s' % self.handle
            return
        self.handle = self.sthread.switch(self.handle)
        if self.handle is self.sthread.get_null_handle():
            print 'language_process thread has finished1'
        if self.sthread.is_empty_handle(self.handle):
            print 'language_process thread has finished2'

    def _has_valid_handle(self):
        # TODO: make less verbose when this proved to work
        if not bool(self.handle):
            print 'handle evaluates to False: %s' % self.handle
            return False
        if self.sthread.is_empty_handle(self.handle):
            print 'handle is empty: %s' % self.handle
            return False
        if self.handle is self.sthread.get_null_handle():
            print 'handle is null handle: %s' % self.handle
            return False
        return True

    @staticmethod
    def new_stacklet_callback(h, arg):
        print 'new_stacklet_callback:', h
        self = global_execution_state.origin
        self.handle = h
        global_execution_state.clear()
        self.language_process().safe_run()
        global_execution_state.origin = self
        return self.handle  # return to Smalltalk when done


class GreenletLanguageRunner(AbstractLanguageRunner):
    def start_thread(self):
        from greenlet import greenlet
        global_execution_state.origin = self
        self.greenlet = greenlet(self.__class__.new_greenlet_callback())
        self.resume()  # stacklets also start immediately

    def resume_thread(self):
        self.greenlet.switch()

    def resumable(self):
        return not self.greenlet.dead

    def yield_thread(self):
        self.greenlet.parent.switch()

    @staticmethod
    def new_greenlet_callback():
        print 'new_greenlet_callback'
        self = global_execution_state.origin
        return self.language_process().safe_run


class GlobalState:
    def clear(self):
        self.origin = None
global_execution_state = GlobalState()
global_execution_state.clear()
