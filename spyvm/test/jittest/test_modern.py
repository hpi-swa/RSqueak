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
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        i92 = int_add(i84, 100)
        i93 = int_add(i84, 1)
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
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        i92 = int_add(i84, 100)
        leave_portal_frame(0)
        leave_portal_frame(0)
        i93 = int_add(i84, 1)
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, i93, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i94, p68, descr=TargetToken(312516328))
        """)

    @py.test.mark.skipif("'Not ready'")
    def test_collection_at(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | o |
        o := OrderedCollection newFrom: #(12 13 123 13 1 123 132 132 123 1 213 123 112  2).
        1 to: 100000 do: [:i | o at: 2].
        """)
        self.assert_matches(traces[0].loop, """
        """)

    @py.test.mark.skipif("'Not ready'")
    def test_collect(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | o |
        o := OrderedCollection newFrom: #(12 13 123 13 1 123 132 132 123 1 213 123 112  2).
        1 to: 100000 do: [:i | o collect: [:e | e * 2]].
        """)
        self.assert_matches(traces[0].loop, """
        """)

    @py.test.mark.skipif("'Not ready'")
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
          a := ((1 to: 10000) do: [:j| j + i]) last bitOr: a
        ].
        """)
        self.assert_matches(traces[0].loop,
        # """
        # guard_not_invalidated(descr=<Guard0xa11ad30>),
        # i72 = int_le(i65, 100000),
        # guard_true(i72, descr=<Guard0xa11ad00>),
        # i73 = int_or(i58, i65),
        # i74 = uint_lt(i73, 2147483647),
        # guard_false(i74, descr=<Guard0xa11acd0>),
        # i75 = int_add(i65, 1),
        # i76 = int_sub(i69, 1),
        # setfield_gc(ConstPtr(ptr66), i76, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 20>),
        # i77 = int_le(i76, 0),
        # guard_false(i77, descr=<Guard0xa11aca0>),
        # jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, p16, p18, i75, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i58, i76, descr=TargetToken(169079000))
        # """
        """
        guard_not_invalidated(descr=<Guard0x158fa220>),
        i133 = int_lt(i77, i58),
        guard_true(i133, descr=<Guard0x158fa1f0>),
        i134 = getarrayitem_gc(p64, 0, descr=<ArrayS 4>),
        i135 = int_eq(i134, 2147483647),
        guard_false(i135, descr=<Guard0x158fa1c0>),
        i136 = getarrayitem_gc(p64, 2, descr=<ArrayS 4>),
        i137 = int_eq(i136, 2147483647),
        guard_false(i137, descr=<Guard0x158fa190>),
        i138 = int_mul_ovf(i77, i136),
        guard_no_overflow(descr=<Guard0x158fa160>),
        i139 = int_add_ovf(i134, i138),
        guard_no_overflow(descr=<Guard0x158fa130>),
        i140 = int_add(i77, 1),
        p141 = getarrayitem_gc(p86, 2, descr=<ArrayP 4>),
        guard_class(p141, ConstClass(W_SmallInteger), descr=<Guard0x158fa100>),
        i142 = getfield_gc_pure(p141, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
        guard_value(i142, 1, descr=<Guard0x158fa0d0>),
        p143 = getarrayitem_gc(p86, 0, descr=<ArrayP 4>),
        guard_class(p143, ConstClass(W_PointersObject), descr=<Guard0x158fa0a0>),
        p144 = getfield_gc_pure(p143, descr=<FieldP spyvm.model.W_AbstractObjectWithClassReference.inst_w_class 12>),
        guard_value(p144, ConstPtr(ptr96), descr=<Guard0x158fa070>),
        p145 = getfield_gc(p143, descr=<FieldP spyvm.model.W_PointersObject.inst_strategy 24>),
        guard_nonnull_class(p145, ConstClass(ContextPartShadow), descr=<Guard0x158fa040>),
        i147 = ptr_eq(p145, p0),
        guard_false(i147, descr=<Guard0x158d4fd0>),
        p148 = getfield_gc(p145, descr=<FieldP spyvm.storage_contexts.ContextPartShadow.vable_token 12>),
        i149 = ptr_ne(p148, ConstPtr(null)),
        cond_call(i149, 1684352, p145, descr=<Callv 0 r EF=2 OS=121>),
        p150 = getfield_gc(p145, descr=<FieldP spyvm.storage_contexts.ContextPartShadow.inst__w_method 44>),
        p151 = getfield_gc(p145, descr=<FieldP spyvm.storage_contexts.ContextPartShadow.inst__w_receiver 48>),
        guard_value(p150, ConstPtr(ptr107), descr=<Guard0x158d4fa0>),
        p152 = getarrayitem_gc(p86, 1, descr=<ArrayP 4>),
        guard_class(p152, ConstClass(W_SmallInteger), descr=<Guard0x158d4f70>),
        i153 = getfield_gc_pure(p152, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
        guard_value(i153, 52, descr=<Guard0x158d4f40>),
        p154 = getarrayitem_gc(p86, 3, descr=<ArrayP 4>),
        enter_portal_frame(0, 0)
        guard_class(p154, ConstClass(W_SmallInteger), descr=<Guard0x158d4f10>),
        i155 = getfield_gc_pure(p154, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
        i156 = int_add_ovf(i139, i155),
        guard_no_overflow(descr=<Guard0x158d4ee0>),
        leave_portal_frame(0)
        i157 = arraylen_gc(p64, descr=<ArrayS 4>),
        jump(p0, p1, i2, p3, p6, p7, i8, i9, p10, p11, i13, p16, i139, i140, p22, i156, p30, p32, p34, p36, p38, p40, p42, p44, p46, p48, i58, p64, p86, descr=TargetToken(361602712))
        """)
