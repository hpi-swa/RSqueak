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

    def test_mixed_stack(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | a |
        a := 0.
        (1 to: 10000) do: [:i|
          a := ((1 to: 10000) do: [:j| j + i]) bitOr: a
        ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xa11ad30>),
        i72 = int_le(i65, 100000),
        guard_true(i72, descr=<Guard0xa11ad00>),
        i73 = int_or(i58, i65),
        i74 = uint_lt(i73, 2147483647),
        guard_false(i74, descr=<Guard0xa11acd0>),
        i75 = int_add(i65, 1),
        i76 = int_sub(i69, 1),
        setfield_gc(ConstPtr(ptr66), i76, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 20>),
        i77 = int_le(i76, 0),
        guard_false(i77, descr=<Guard0xa11aca0>),
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, p16, p18, i75, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i58, i76, descr=TargetToken(169079000))
        """)
