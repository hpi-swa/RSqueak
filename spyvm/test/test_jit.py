import sys
from pypy.jit.metainterp.test.test_basic import LLJitMixin
from pypy.rlib.jit import OPTIMIZER_FULL, OPTIMIZER_SIMPLE

from spyvm.test import test_miniimage
from spyvm.test.test_miniimage import perform, w

def setup_module(mod):
    test_miniimage.setup_module(test_miniimage)
    mod.space = test_miniimage.space


class TestLLtype(LLJitMixin):
    def test_compiled_method(self):
        sourcecode = """foo
                            | x |
                            x := self.
                            [ x < 100 ] whileTrue: [ x := x + 1 ].
                            ^x"""
        perform(w(10).getclass(space), "compile:classified:notifying:", w(sourcecode), w('pypy'), w(None))

        w_10 = w(10)
        w_100 = w(100)

        def interp_w():
            assert perform(w_10, "foo").is_same_object(w_100)

        # XXX ah ha ha!
        sys.setrecursionlimit(100000)

        from pypy.conftest import option
        option.view = False
        option.viewloops = True
        self.meta_interp(interp_w, [], listcomp=True, backendopt=True,
                         listops=True, optimizer=OPTIMIZER_FULL)
