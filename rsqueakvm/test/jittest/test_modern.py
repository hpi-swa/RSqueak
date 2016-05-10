import py

from .base import ModernJITTest

class TestModern(ModernJITTest):
    def test_named_access(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | m |
        m := Morph new.
        1 to: 100000 do: [:i | m bounds ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xdb8da34>)
        i70 = int_le(i69, 100000)
        guard_true(i70, descr=<Guard0xdc9c7f0>)
        i71 = int_add(i69, 1)
        i72 = arraylen_gc(p65, descr=<ArrayP 4>)
        jump(p0, p1, i2, p3, p6, p7, i8, i9, p10, p11, i13, p14, p17, i71, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p45, p47, p65, descr=TargetToken(231309508))
        """)

    def test_named_access_in_array(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | o |
        o := Array with: Morph new.
        1 to: 100000 do: [:i | (o at: 1) bounds ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xdb8da34>)
        i70 = int_le(i69, 100000)
        guard_true(i70, descr=<Guard0xdc9c7f0>)
        i71 = int_add(i69, 1)
        i72 = arraylen_gc(p65, descr=<ArrayP 4>)
        jump(p0, p1, i2, p3, p6, p7, i8, i9, p10, p11, i13, p14, p17, i71, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p45, p47, p65, descr=TargetToken(231309508))
        """)

    @py.test.mark.skipif("'Not ready'")
    def test_named_access_in_do_block(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | o |
        o := Array with: Morph new.
        1 to: 100000 do: [:i | o do: [:m | m bounds ] ].
        """)
        self.assert_matches(traces[1].loop, """
        guard_not_invalidated(descr=<Guard0xdb8da34>)
        i70 = int_le(i69, 100000)
        guard_true(i70, descr=<Guard0xdc9c7f0>)
        i71 = int_add(i69, 1)
        i72 = arraylen_gc(p65, descr=<ArrayP 4>)
        i73 = arraylen_gc(p67, descr=<ArrayP 4>)
        jump(p0, p1, i2, p3, p6, p7, i8, i9, p10, p11, i13, p14, p17, i71, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p45, p47, p65, descr=TargetToken(231309508))
        """)

    def test_named_access_fresh(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        1 to: 100000 do: [:i | Morph new bounds ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xe445c70>)
        i169 = int_le(i162, 100000)
        guard_true(i169, descr=<Guard0xe5ec7b8>)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        i170 = int_add(i162, 1)
        i171 = int_sub(i166, 1)
        setfield_gc(ConstPtr(ptr163), i171, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 20>)
        i172 = int_le(i171, 0)
        guard_false(i172, descr=<Guard0xe445c9c>)
        i174 = arraylen_gc(p61, descr=<ArrayP 4>)
        i175 = arraylen_gc(p68, descr=<ArrayP 4>)
        i176 = arraylen_gc(p94, descr=<ArrayP 4>)
        i177 = arraylen_gc(p112, descr=<ArrayP 4>)
        i178 = arraylen_gc(p140, descr=<ArrayP 4>)
        jump(p0, p1, i2, p3, p6, p7, i8, i9, p10, p11, i13, p14, i170, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p45, p47, p61, p68, p63, p94, p112, p114, p140, p142, p154, i171, descr=TargetToken(241143860))
        """)

    def test_named_access_and_send(self, spy, squeak, tmpdir):
        traces = self.run(spy, squeak, tmpdir, """
        | m |
        m := Morph new.
        1 to: 100000 do: [:i | m bounds outsetBy: 10 ].
        """)
        self.assert_matches(traces[3].loop, """
        guard_not_invalidated(descr=<Guard0xdba73ac>),
        i234 = int_le(i227, 100000)
        guard_true(i234, descr=<Guard0xdc6f080>)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        enter_portal_frame(0, 0)
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        i235 = int_add(i227, 1)
        i236 = int_sub(i231, 3)
        setfield_gc(ConstPtr(ptr228), i236, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 20>)
        i237 = int_le(i236, 0)
        guard_false(i237, descr=<Guard0xdba7380>)
        i239 = arraylen_gc(p65, descr=<ArrayP 4>)
        i240 = arraylen_gc(p86, descr=<ArrayP 4>)
        i241 = arraylen_gc(p89, descr=<ArrayP 4>)
        i242 = arraylen_gc(p97, descr=<ArrayP 4>)
        i243 = arraylen_gc(p119, descr=<ArrayP 4>)
        i245 = int_sub_ovf(i147, 10)
        guard_no_overflow(descr=<Guard0xdba7354>)
        i246 = int_sub_ovf(i156, 10)
        guard_no_overflow(descr=<Guard0xdba7328>)
        i244 = arraylen_gc(p145, descr=<ArrayS 4>)
        jump(p0, p1, i2, p3, p6, p7, i8, i9, p10, p11, i13, p14, p17, i235, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p45, p47, p65, p67, p86, p89, p97, p91, p119, p121, p145, p169, p189, p88, i236, i147, i156, descr=TargetToken(231130940))
        """)

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
        # setfield_gc(ConstPtr(ptr27), 10000, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 20>),
        # guard_no_exception(descr=<Guard0x9161a60>),
        # f30 = float_sub(f26, 1424262084.583439),
        # f32 = float_mul(f30, 1000.000000),
        # i33 = cast_float_to_int(f32),
        # i35 = int_and(i33, 2147483647),
        # i36 = getfield_gc(ConstPtr(ptr27), descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_next_wakeup_tick 28>),
        # i37 = int_is_zero(i36),
        # guard_true(i37, descr=<Guard0x9161bb0>),
        # guard_nonnull(p0, descr=<Guard0x9161b80>),
        # i39 = int_le(i24, 1000000001),
        # guard_true(i39, descr=<Guard0x9161b50>),
        # i40 = getfield_gc_pure(p0, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst_is_block_context 69>),
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
        """
        guard_not_invalidated(descr=<Guard0xd7bd2fc>)
        i133 = int_lt(i76, i58)
        guard_true(i133, descr=<Guard0xd96b630>)
        i134 = int_mul_ovf(i76, i70)
        guard_no_overflow(descr=<Guard0xd96b614>)
        i135 = int_add_ovf(i66, i134)
        guard_no_overflow(descr=<Guard0xd96b5dc>)
        i136 = int_add(i76, 1)
        cond_call(i101, 137066992, p96, descr=<Callv 0 r EF=2 OS=121>)
        enter_portal_frame(0, 0)
        i137 = int_add_ovf(i135, i130)
        guard_no_overflow(descr=<Guard0xd7bd2d0>)
        leave_portal_frame(0)
        i139 = arraylen_gc(p64, descr=<ArrayS 4>)
        jump(p0, p1, i2, p3, p6, p7, i8, i9, p10, p11, i13, p14, p17, i135, i136, p23, i137, p31, p33, p35, p37, p39, p41, p43, p45, p47, p49, i58, p64, i70, i66, p85, p92, p96, i101, p78, i130, p123, p104, descr=TargetToken(227905452))
        """)

    @py.test.mark.skipif("'Flaky, check with pypy devs'")
    def test_benchFib(self, spy, squeak, tmpdir):
        """Tests how well call_assembler and int-local-return works"""
        traces = self.run(spy, squeak, tmpdir, """
        25 benchFib
        """)
        self.assert_matches(traces[0].bridges[-1], """
        guard_value(i7, 0, descr=<Guard0xa655430>),
        guard_not_invalidated(descr=<Guard0xa67d4d0>),
        guard_value(p1, ConstPtr(ptr28), descr=<Guard0xa67d4b0>),
        guard_value(i2, 0, descr=<Guard0xa67d470>),
        guard_class(p8, ConstClass(W_SmallInteger), descr=<Guard0xa67d3f0>),
        i31 = getfield_gc_i(p8, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>),
        i33 = int_lt(i31, 2),
        guard_false(i33, descr=<Guard0xa67d3b0>),
        i35 = int_sub(i31, 1),
        i36 = getfield_gc_i(p0, descr=<FieldU rsqueakvm.storage.AbstractStrategy.inst_space 8 pure>),
        p38 = getfield_gc_r(ConstPtr(ptr37), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 20>),
        guard_value(p38, ConstPtr(ptr39), descr=<Guard0xa655730>),
        p41 = getfield_gc_r(ConstPtr(ptr40), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 20>),
        guard_value(p41, ConstPtr(ptr42), descr=<Guard0xa655700>),
        p43 = new_with_vtable(descr=<SizeDescr 64>),
        p45 = new_array_clear(16, descr=<ArrayP 4>),
        setarrayitem_gc(p45, 0, ConstPtr(ptr47), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 1, ConstPtr(ptr49), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 2, ConstPtr(ptr51), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 3, ConstPtr(ptr53), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 4, ConstPtr(ptr55), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 5, ConstPtr(ptr57), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 6, ConstPtr(ptr59), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 7, ConstPtr(ptr61), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 8, ConstPtr(ptr63), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 9, ConstPtr(ptr65), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 10, ConstPtr(ptr67), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 11, ConstPtr(ptr69), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 12, ConstPtr(ptr71), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 13, ConstPtr(ptr73), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 14, ConstPtr(ptr75), descr=<ArrayP 4>),
        setarrayitem_gc(p45, 15, ConstPtr(ptr77), descr=<ArrayP 4>),
        p78 = new_with_vtable(descr=<SizeDescr 12>),
        setfield_gc(p78, i35, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>),
        setfield_gc(p43, 6371376, descr=<FieldU rsqueakvm.storage.AbstractStrategy.inst_space 8 pure>),
        setfield_gc(p43, ConstPtr(ptr80), descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 12 pure>),
        setfield_gc(p43, 0, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__pc 20>),
        setfield_gc(p43, p0, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>),
        setfield_gc(p43, 0, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__stack_ptr 28>),
        setfield_gc(p43, p45, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__temps_and_stack 32>),
        setfield_gc(p43, ConstPtr(ptr83), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 36>),
        setfield_gc(p43, p78, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_receiver 40>),
        setfield_gc(p43, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self 44>),
        setfield_gc(p43, 22, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self_size 48>),
        setfield_gc(p43, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 52>),
        setfield_gc(p43, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_extra_data 56>),
        setfield_gc(p43, ConstPtr(ptr88), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_state 60>),
        call_assembler_n(p43, descr=<Loop0>),
        guard_not_forced(descr=<Guard0xa4bfcdc>),
        keepalive(p43),
        p90 = guard_exception(4706660, descr=<Guard0xa6554c0>),
        i91 = ptr_eq(p43, p0),
        guard_false(i91, descr=<Guard0xa6556d0>),
        p92 = getfield_gc_r(p43, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>),
        i94 = ptr_ne(p92, ConstPtr(null)),
        cond_call(i94, 1442416, p43, descr=<Callv 0 r EF=2 OS=121>),
        p96 = getfield_gc_r(p43, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_state 60>),
        i98 = instance_ptr_eq(p96, ConstPtr(ptr97)),
        guard_false(i98, descr=<Guard0xa6556a0>),
        guard_not_invalidated(descr=<Guard0xa67d2b0>),
        p99 = getfield_gc_r(p43, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 52>),
        guard_isnull(p99, descr=<Guard0xa67d290>),
        p100 = getfield_gc_r(p43, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 36>),
        guard_value(p100, ConstPtr(ptr101), descr=<Guard0xa67d270>),
        p102 = getfield_gc_r(p43, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>),
        setfield_gc(p43, -1, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__pc 20>),
        guard_nonnull(p102, descr=<Guard0xa655670>),
        setfield_gc(p43, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>),
        guard_class(p90, 4706660, descr=<Guard0xa655640>),
        i106 = getfield_gc_i(p90, descr=<FieldS rsqueakvm.interpreter.IntLocalReturn.inst__value 8 pure>),
        i108 = int_sub(i31, 2),
        p110 = getfield_gc_r(ConstPtr(ptr109), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 20>),
        setfield_gc(p43, ConstPtr(ptr111), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_state 60>),
        guard_value(p110, ConstPtr(ptr112), descr=<Guard0xa655610>),
        p114 = getfield_gc_r(ConstPtr(ptr113), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 20>),
        guard_value(p114, ConstPtr(ptr115), descr=<Guard0xa6555e0>),
        p116 = new_with_vtable(descr=<SizeDescr 64>),
        p118 = new_array_clear(16, descr=<ArrayP 4>),
        setarrayitem_gc(p118, 0, ConstPtr(ptr120), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 1, ConstPtr(ptr122), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 2, ConstPtr(ptr124), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 3, ConstPtr(ptr126), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 4, ConstPtr(ptr128), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 5, ConstPtr(ptr130), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 6, ConstPtr(ptr132), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 7, ConstPtr(ptr134), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 8, ConstPtr(ptr136), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 9, ConstPtr(ptr138), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 10, ConstPtr(ptr140), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 11, ConstPtr(ptr142), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 12, ConstPtr(ptr144), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 13, ConstPtr(ptr146), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 14, ConstPtr(ptr148), descr=<ArrayP 4>),
        setarrayitem_gc(p118, 15, ConstPtr(ptr150), descr=<ArrayP 4>),
        p151 = new_with_vtable(descr=<SizeDescr 12>),
        setfield_gc(p151, i108, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>),
        setfield_gc(p116, 6371376, descr=<FieldU rsqueakvm.storage.AbstractStrategy.inst_space 8 pure>),
        setfield_gc(p116, ConstPtr(ptr153), descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 12 pure>),
        setfield_gc(p116, 0, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__pc 20>),
        setfield_gc(p116, p0, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>),
        setfield_gc(p116, 0, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__stack_ptr 28>),
        setfield_gc(p116, p118, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__temps_and_stack 32>),
        setfield_gc(p116, ConstPtr(ptr156), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 36>),
        setfield_gc(p116, p151, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_receiver 40>),
        setfield_gc(p116, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self 44>),
        setfield_gc(p116, 22, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self_size 48>),
        setfield_gc(p116, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 52>),
        setfield_gc(p116, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_extra_data 56>),
        setfield_gc(p116, ConstPtr(ptr161), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_state 60>),
        call_assembler_n(p116, descr=<Loop0>),
        guard_not_forced(descr=<Guard0xa4bfca8>),
        keepalive(p116),
        p163 = guard_exception(4706660, descr=<Guard0xa655490>),
        i164 = ptr_eq(p116, p0),
        guard_false(i164, descr=<Guard0xa6555b0>),
        p165 = getfield_gc_r(p116, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>),
        i167 = ptr_ne(p165, ConstPtr(null)),
        cond_call(i167, 1442416, p116, descr=<Callv 0 r EF=2 OS=121>),
        p169 = getfield_gc_r(p116, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_state 60>),
        i171 = instance_ptr_eq(p169, ConstPtr(ptr170)),
        guard_false(i171, descr=<Guard0xa655580>),
        guard_not_invalidated(descr=<Guard0xa67d150>),
        p172 = getfield_gc_r(p116, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 52>),
        guard_isnull(p172, descr=<Guard0xa67d130>),
        p173 = getfield_gc_r(p116, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 36>),
        guard_value(p173, ConstPtr(ptr174), descr=<Guard0xa67d110>),
        p175 = getfield_gc_r(p116, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>),
        setfield_gc(p116, -1, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__pc 20>),
        guard_nonnull(p175, descr=<Guard0xa655550>),
        setfield_gc(p116, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>),
        guard_class(p163, 4706660, descr=<Guard0xa655520>),
        i179 = getfield_gc_i(p163, descr=<FieldS rsqueakvm.interpreter.IntLocalReturn.inst__value 8 pure>),
        setfield_gc(p116, ConstPtr(ptr180), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_state 60>),
        i181 = int_add_ovf(i106, i179),
        guard_no_overflow(descr=<Guard0xa6554f0>),
        i184 = int_add_ovf(i181, 1),
        guard_no_overflow(descr=<Guard0xa67d050>),
        guard_isnull(p4, descr=<Guard0xa67d010>),
        i186 = instance_ptr_eq(p6, ConstPtr(ptr185)),
        guard_false(i186, descr=<Guard0xa665fd0>),
        leave_portal_frame(4),
        p188 = force_token(),
        setfield_gc(p0, p188, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>),
        p189 = new_with_vtable(descr=<SizeDescr 12>),
        setfield_gc(p189, i184, descr=<FieldS rsqueakvm.interpreter.IntLocalReturn.inst__value 8 pure>),
        guard_not_forced_2(descr=<Guard0xa4bfc74>),
        finish(p189, descr=<ExitFrameWithExceptionDescrRef object at 0x5835f8>)
        """)
