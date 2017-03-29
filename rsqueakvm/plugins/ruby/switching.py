class InterruptCounter:
    counter_size = 10000

    def __init__(self):
        self.reset()

    def reset(self):
        self._counter = self.counter_size

    def triggers(self, decr_by=1):
        self._counter -= decr_by
        if self._counter <= 0:
            self._counter = self.counter_size
            return True
        return False


interrupt_counter = InterruptCounter()


def switch_to_smalltalk(ruby_process):
    # import pdb; pdb.set_trace()
    if ruby_process is None:
        return
    runner = ruby_process.runner()
    if runner is None:
        return

    # print 'Ruby yield'
    runner.return_to_smalltalk()
    # print 'Ruby continue'

    # error has been in Smalltalk land, clear it now to allow resuming
    ruby_process.reset_error()
