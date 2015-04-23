import py

from .base import ModernJITTest

class TestModern(ModernJITTest):
    def test_simple_loop_with_closure(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        1 to: 100000 do: [:i | [i] value + 100].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x12a02f10>)
        i91 = int_le(i84, 100000)
        guard_true(i91, descr=<Guard0x12a02ee0>)
        i92 = int_add(i84, 100)
        i93 = int_add(i84, 1)
        i97 = arraylen_gc(p68, descr=<ArrayU 1>)
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, i93, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i94, p68, descr=TargetToken(312516328))
        """)
        # self.assert_matches(traces[0].bridges[0], """
        # f26 = call(ConstClass(ll_time.ll_time_time), descr=<Callf 8 EF=4>),
        # setfield_gc(ConstPtr(ptr27), 10000, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 20>),
        # guard_no_exception(descr=<Guard0x9161a60>),
        # f30 = float_sub(f26, 1424262084.583439),
        # f32 = float_mul(f30, 1000.000000),
        # i33 = cast_float_to_int(f32),
        # i35 = int_and(i33, 2147483647),
        # i36 = getfield_gc(ConstPtr(ptr27), descr=<FieldS spyvm.interpreter.Interpreter.inst_next_wakeup_tick 28>),
        # i37 = int_is_zero(i36),
        # guard_true(i37, descr=<Guard0x9161bb0>),
        # guard_nonnull(p0, descr=<Guard0x9161b80>),
        # i39 = int_le(i24, 1000000001),
        # guard_true(i39, descr=<Guard0x9161b50>),
        # i40 = getfield_gc_pure(p0, descr=<FieldU spyvm.storage_contexts.ContextPartShadow.inst_is_block_context 69>),
        # guard_value(i40, 0, descr=<Guard0x9161b20>),
        # jump(p0, p1, p2, i3, i4, p5, i6, i7, p8, p9, p10, i24, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, 10000, descr=TargetToken(152597176))
        # """)

    def test_block_passing(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        1 to: 100000 do: [:i | [:blk | blk value: i] value: [:x | x + 100]].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x12a02f10>)
        i91 = int_le(i84, 100000)
        guard_true(i91, descr=<Guard0x12a02ee0>)
        i92 = int_add(i84, 100)
        i93 = int_add(i84, 1)
        i97 = arraylen_gc(p68, descr=<ArrayU 1>)
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, i93, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i94, p68, descr=TargetToken(312516328))
        """)

    def test_collection_at(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | o |
        o := OrderedCollection newFrom: #(12 13 123 13 1 123 132 132 123 1 213 123 112  2).
        1 to: 100000 do: [:i | o at: 2].
        """)
        self.assert_matches(traces[0].loop, """
        """)

    def test_collect(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | o |
        o := OrderedCollection newFrom: #(12 13 123 13 1 123 132 132 123 1 213 123 112  2).
        1 to: 100000 do: [:i | o collect: [:e | e * 2]].
        """)
        self.assert_matches(traces[0].loop, """
        """)

    def test_inject(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | o |
        o := OrderedCollection newFrom: #(12 13 123 13 1 123 132 132 123 1 213 123 112  2).
        1 to: 100000 do: [:i | o inject: 0 into: [:sum :e | e + sum]].
        """)
        self.assert_matches(traces[0].loop, """
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
