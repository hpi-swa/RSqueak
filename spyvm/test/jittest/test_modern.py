import py

from .base import ModernJITTest

class TestModern(ModernJITTest):
    def test_simple_loop(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        1 to: 100000 do: [:i | [i] value + 100].
        """)
        self.assert_matches(traces[0].loop, """
         guard_not_invalidated(descr=<Guard0x910a1f0>),
        """)

    def test_more(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        1 to: 100000 do: [:i | [i * 12] value + 100].
        """)
        self.assert_matches(traces[0].loop, """
         guard_not_invalidated(descr=<Guard0x910a1f0>),
        """)
