from rsqueakvm.plugins.foreign_language.process import W_ForeignLanguageProcess
from rsqueakvm.plugins.ruby.frame import WR_FrameObject
from rsqueakvm.plugins.ruby.model import W_RubyObject
from rsqueakvm.plugins.ruby.objspace import ruby_space

from topaz.error import RubyError
from topaz.executioncontext import ExecutionContext


class W_RubyProcess(W_ForeignLanguageProcess):
    _attrs_ = ['source', 'filepath', 'ec']
    repr_classname = 'W_RubyProcess'

    def __init__(self, source, filepath='-e', break_on_exceptions=True):
        W_ForeignLanguageProcess.__init__(self, break_on_exceptions)
        self.source = source
        self.filepath = filepath
        self.ec = ExecutionContext()

    def run(self):
        print 'Ruby start'
        try:
            retval = ruby_space.execute(self.source, filepath=self.filepath)
            self.set_result(W_RubyObject(retval))
        except RubyError as e:
            self.set_result(W_RubyObject(e.w_value))

    def set_current(self):
        ruby_space.current_ruby_process.set(self)

    def w_top_frame(self):
        if self.ec is None:
            return None
        topframe = self.ec.gettoprubyframe()
        if topframe is None:
            return None
        return W_RubyObject(WR_FrameObject(topframe))
