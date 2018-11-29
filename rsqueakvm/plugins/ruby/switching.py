import os

from rsqueakvm.util.cells import QuasiConstant
from rsqueakvm.plugins.foreign_language.utils import log


class InterruptCounter:
    def __init__(self):
        self.counter_size = QuasiConstant(10000)
        self.interrupts_disabled = QuasiConstant(False)
        self.reset()

    def setup(self):
        try:
            self.counter_size.set(int(
                os.environ.get('TOPAZ_CHECK_INTERVAL') or '10000'))
            self.interrupts_disabled.set(bool(
                os.environ.get('TOPAZ_DISABLE_INTERRUPTS')))
        except:
            pass
        if self.interrupts_disabled.get():
            log('TOPAZ_DISABLE_INTERRUPTS set.')
        else:
            log('TOPAZ_CHECK_INTERVAL set to %s.' % self.counter_size.get())

    def reset(self):
        self._counter = self.counter_size.get()

    def triggers(self, decr_by=1):
        if self.interrupts_disabled.get():
            return False
        self._counter -= decr_by
        if self._counter <= 0:
            self._counter = self.counter_size.get()
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
