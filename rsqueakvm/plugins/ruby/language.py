from rsqueakvm.plugins.foreign_language.language import W_ForeignLanguage
from rsqueakvm.plugins.ruby.model import W_RubyObject
from rsqueakvm.plugins.ruby.objspace import ruby_space

from topaz.error import RubyError


class W_RubyLanguage(W_ForeignLanguage):
    _attrs_ = ['source']
    repr_classname = 'W_RubyLanguage'

    def __init__(self, source, break_on_exceptions=True):
        W_ForeignLanguage.__init__(self, break_on_exceptions)
        self.source = source

    def run(self):
        print 'Ruby start'
        try:
            retval = ruby_space.execute(self.source)
            self.set_result(retval)
        except RubyError as e:
            self.set_result(e.w_value)

    def set_current(self):
        ec = ruby_space.getexecutioncontext()
        ec.current_language = self

    def set_result(self, wr_result):
        self.w_result = W_RubyObject(wr_result)

    def set_error(self, wr_error):
        self.w_error = W_RubyObject(wr_error)
