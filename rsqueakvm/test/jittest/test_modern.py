import py

from .base import ModernJITTest

from rsqueakvm.util.system import IS_64BIT

class TestModern(ModernJITTest):
    def test_init(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | c |
        Object
            subclass: #MyA
            instanceVariableNames: 'i'
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := Smalltalk at: #MyA.
        c compile: 'initialize
i := 1.' classified: 'none' withStamp: nil notifying: nil logSource: false.
        1 to: 100000 do: [:i | c new ].
        """)
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0xb06c3c8>)
        i88 = int_le(i79, 100000)
        guard_true(i88, descr=<Guard0xb051148>)
        p89 = force_token()
        enter_portal_frame(4, 0)
        p92 = force_token()
        enter_portal_frame(4, 0)
        leave_portal_frame(4)
        leave_portal_frame(4)
        i98 = int_add(i79, 1)
        i100 = int_sub(i83, 1)
        setfield_gc(ConstPtr(ptr101), i100, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i103 = int_le(i100, 0)
        guard_false(i103, descr=<Guard0xb06c498>)
        jump(p0, p1, i2, p3, p4, p7, p8, p10, p13, i98, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i100, descr=TargetToken(183580560))
        """)

    def test_ivar_access(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | c o |
        Object
            subclass: #MyA
            instanceVariableNames: 'i'
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := Smalltalk at: #MyA.
        c compile: 'initialize
i := 1.' classified: 'none' withStamp: nil notifying: nil logSource: false.
        c compile: 'i
^ i' classified: 'none' withStamp: nil notifying: nil logSource: false.
        c compile: 'i: n
i := n' classified: 'none' withStamp: nil notifying: nil logSource: false.
        o := c new.
        [ o i < 10000 ] whileTrue: [ o i: o i + 1 ].
        """)
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0x3f07cf8>)
        i70 = int_lt(i54, 10000)
        guard_true(i70, descr=<Guard0x5fcfda8>)
        i72 = int_add(i54, 1)
        p73 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        i78 = int_sub(i65, 1)
        setfield_gc(p13, i72, descr=<FieldS rsqueakvm.model.pointers.W_FixedPointersObject.inst__intField1 40>)
        setfield_gc(ConstPtr(ptr79), i78, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i81 = int_le(i78, 0)
        guard_false(i81, descr=<Guard0x3f07d60>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, p13, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i72, i78, descr=TargetToken(105649344))
        """)

    def test_named_access(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | m |
        m := Morph new.
        1 to: 100000 do: [:i | m bounds ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x4260be8>)
        i65 = int_le(i56, 100000)
        guard_true(i65, descr=<Guard0x5d52890>)
        i67 = int_add(i56, 1)
        i69 = int_sub(i60, 1)
        setfield_gc(ConstPtr(ptr70), i69, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i72 = int_le(i69, 0)
        guard_false(i72, descr=<Guard0x4260c50>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i67, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i69, descr=TargetToken(97209184))
        """)

    def test_named_access_in_array(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | o |
        o := Array with: Morph new.
        1 to: 100000 do: [:i | (o at: 1) bounds ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x4c0f338>)
        i80 = int_le(i71, 100000)
        guard_true(i80, descr=<Guard0x66fe920>)
        i82 = int_add(i71, 1)
        i84 = int_sub(i75, 1)
        setfield_gc(ConstPtr(ptr85), i84, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i87 = int_le(i84, 0)
        guard_false(i87, descr=<Guard0x4c0f3a0>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i82, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p57, p62, i84, descr=TargetToken(107351760))
        """)

    def test_named_access_in_do_block(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | o |
        o := Array with: Morph new.
        1 to: 100000 do: [:i | o do: [:m | m bounds ] ].
        """)
        self.assert_matches(traces[1].loop, """
        guard_not_invalidated(descr=<Guard0x4a761c0>)
        i150 = int_le(i141, 100000)
        guard_true(i150, descr=<Guard0x65e77c0>)
        p151 = getfield_gc_r(p1, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_nonnull_class(p151, ConstClass(ContextPartShadow), descr=<Guard0x4a76228>)
        i153 = ptr_eq(p151, p0)
        guard_true(i153, descr=<Guard0x65e7808>)
        p155 = getfield_gc_r(ConstPtr(ptr154), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p155, ConstPtr(ptr156), descr=<Guard0x4a76290>)
        p157 = getfield_gc_r(p11, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p157, ConstPtr(ptr158), descr=<Guard0x4a762f8>)
        p160 = getfield_gc_r(ConstPtr(ptr159), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p160, ConstPtr(ptr161), descr=<Guard0x4a76360>)
        p162 = force_token()
        enter_portal_frame(1, 0)
        p165 = getfield_gc_r(p11, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 24>)
        i166 = arraylen_gc(p165, descr=<ArrayP 8>)
        i168 = int_le(1, i166)
        guard_true(i168, descr=<Guard0x65e7850>)
        i170 = uint_lt(0, i166)
        guard_true(i170, descr=<Guard0x65e7898>)
        p172 = getarrayitem_gc_r(p165, 0, descr=<ArrayP 8>)
        guard_nonnull_class(p172, ConstClass(W_FixedPointersObject), descr=<Guard0x4a763c8>)
        p174 = force_token()
        enter_portal_frame(1, 0)
        p177 = getfield_gc_r(p172, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p177, ConstPtr(ptr178), descr=<Guard0x4a76430>)
        p180 = getfield_gc_r(ConstPtr(ptr179), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p180, ConstPtr(ptr181), descr=<Guard0x4a76498>)
        p182 = getfield_gc_r(p172, descr=<FieldP rsqueakvm.model.pointers.W_FixedPointersObject.inst__field2 48>)
        guard_nonnull_class(p182, ConstClass(W_FixedPointersObject), descr=<Guard0x4a76568>)
        leave_portal_frame(1)
        i186 = int_sub(i145, 1)
        setfield_gc(ConstPtr(ptr187), i186, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i189 = int_le(i186, 0)
        guard_false(i189, descr=<Guard0x4a765d0>)
        p190 = force_token()
        p191 = new_with_vtable(descr=<SizeDescr 96>)
        p192 = new_with_vtable(descr=<SizeDescr 24>)
        setfield_gc(p192, p162, descr=<FieldP JitVirtualRef.virtual_token 8>)
        setfield_gc(p192, ConstPtr(null), descr=<FieldP JitVirtualRef.forced 16>)
        p194 = new_array_clear(17, descr=<ArrayP 8>)
        p195 = new_with_vtable(descr=<SizeDescr 80>)
        setfield_gc(p195, 0, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8 pure>)
        setfield_gc(p195, ConstPtr(ptr197), descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__stack 24 pure>)
        setfield_gc(p195, 268435510, descr=<FieldU rsqueakvm.model.block_closure.W_BlockClosure.inst__startpc_stacklen_args 32 pure>)
        setfield_gc(p195, ConstPtr(ptr199), descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__w_method 40 pure>)
        setfield_gc(p195, p1, descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__w_outerContext 48 pure>)
        setfield_gc(p195, p6, descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__w_receiver 56 pure>)
        setfield_gc(p195, ConstPtr(ptr200), descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst_version 64 pure>)
        setarrayitem_gc(p194, 0, p195, descr=<ArrayP 8>)
        p202 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p202, 2, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p194, 1, p202, descr=<ArrayP 8>)
        p205 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p205, i166, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p194, 2, p205, descr=<ArrayP 8>)
        setarrayitem_gc(p194, 3, p202, descr=<ArrayP 8>)
        setarrayitem_gc(p194, 4, ConstPtr(ptr209), descr=<ArrayP 8>)
        setarrayitem_gc(p194, 5, ConstPtr(ptr211), descr=<ArrayP 8>)
        setfield_gc(p0, p190, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        setfield_gc(p191, ConstPtr(ptr212), descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        setfield_gc(p191, p192, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>)
        setfield_gc(p191, 1086324741, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        setfield_gc(p191, p194, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__temps_and_stack 40>)
        setfield_gc(p191, ConstPtr(ptr214), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 48>)
        setfield_gc(p191, p11, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_receiver 56>)
        setfield_gc(p191, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self 64>)
        setfield_gc(p191, 23, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self_size 72>)
        setfield_gc(p191, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 80>)
        setfield_gc(p191, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_extra_data 88>)
        call_assembler_n(p191, descr=<Loop2>)
        guard_not_forced(descr=<Guard0x9d293d0>)
        keepalive(p191)
        p220 = guard_exception(13757968, descr=<Guard0x4a76638>)
        i221 = ptr_eq(p191, p0)
        guard_false(i221, descr=<Guard0x4a766a0>)
        p222 = getfield_gc_r(p191, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        i224 = ptr_ne(p222, ConstPtr(null))
        cond_call(i224, 6399472, p191, descr=<Callv 0 r EF=2 OS=121>)
        i226 = getfield_gc_i(p191, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        guard_value(i226, 1086324759, descr=<Guard0x4a76708>)
        guard_not_invalidated(descr=<Guard0x65e78e0>)
        p228 = getfield_gc_r(p191, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 80>)
        guard_isnull(p228, descr=<Guard0x65e7928>)
        p229 = getfield_gc_r(p191, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 48>)
        guard_value(p229, ConstPtr(ptr230), descr=<Guard0x4a76770>)
        setfield_gc(p191, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>)
        setfield_gc(p191, 1090519039, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        guard_class(p220, 13757968, descr=<Guard0x4a767d8>)
        p234 = getfield_gc_r(p220, descr=<FieldP rsqueakvm.interpreter.WrappedLocalReturn.inst_w_value 8 pure>)
        setfield_gc(p191, 16777215, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        setfield_gc(p192, ConstPtr(null), descr=<FieldP JitVirtualRef.virtual_token 8>)
        guard_nonnull(p234, descr=<Guard0x4a76840>)
        i237 = int_add(i141, 1)
        i239 = getfield_gc_i(ConstPtr(ptr238), descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i241 = int_sub(i239, 1)
        setfield_gc(ConstPtr(ptr242), i241, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i244 = int_le(i241, 0)
        guard_false(i244, descr=<Guard0x4a768a8>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i237, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i241, descr=TargetToken(106207040))
        """)

    def test_named_access_fresh(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        1 to: 100000 do: [:i | Morph new bounds ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x3d67748>)
        i163 = int_le(i153, 100000)
        guard_true(i163, descr=<Guard0x58d6f08>)
        p164 = force_token()
        enter_portal_frame(1, 0)
        p167 = force_token()
        enter_portal_frame(1, 0)
        p170 = force_token()
        enter_portal_frame(1, 0)
        p173 = force_token()
        enter_portal_frame(1, 0)
        p176 = force_token()
        enter_portal_frame(1, 0)
        p179 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        leave_portal_frame(1)
        leave_portal_frame(1)
        p186 = force_token()
        enter_portal_frame(1, 0)
        p189 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        leave_portal_frame(1)
        leave_portal_frame(1)
        i197 = int_add(i153, 1)
        i199 = int_sub(i157, 1)
        setfield_gc(ConstPtr(ptr200), i199, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i202 = int_le(i199, 0)
        guard_false(i202, descr=<Guard0x3d677b0>)
        jump(p0, p1, i2, p4, p5, p6, p8, i197, p17, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p52, p99, p128, p130, i199, descr=TargetToken(92509376))
        """)

    def test_named_access_and_send(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | m |
        m := Morph new.
        1 to: 100000 do: [:i | m bounds outsetBy: 10 ].
        """)
        self.assert_matches(traces[3].loop, """
        guard_not_invalidated(descr=<Guard0x3101d60>)
        i149 = int_le(i140, 100000)
        guard_true(i149, descr=<Guard0x579ba90>)
        p150 = force_token()
        enter_portal_frame(1, 0)
        p153 = force_token()
        enter_portal_frame(1, 0)
        p156 = force_token()
        enter_portal_frame(1, 0)
        p159 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        leave_portal_frame(1)
        p165 = force_token()
        enter_portal_frame(1, 0)
        p168 = force_token()
        enter_portal_frame(1, 0)
        p171 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        leave_portal_frame(1)
        p177 = force_token()
        enter_portal_frame(1, 0)
        p180 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        leave_portal_frame(1)
        i187 = int_add(i140, 1)
        i189 = int_sub(i144, 3)
        setfield_gc(ConstPtr(ptr190), i189, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i192 = int_le(i189, 0)
        guard_false(i192, descr=<Guard0x3101cf8>)
        i194 = int_sub_ovf(i92, 10)
        guard_no_overflow(descr=<Guard0x3101c90>)
        i195 = int_sub_ovf(i95, 10)
        guard_no_overflow(descr=<Guard0x3101c28>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i187, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p54, p73, p101, p71, i189, i92, i95, descr=TargetToken(91217200))
        """)

    def test_simple_loop_with_closure(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        1 to: 100000 do: [:i | [i] value + 100].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x12a02f10>)
        i91 = int_le(i84, 100000)
        guard_true(i91, descr=<Guard0x12a02ee0>)
        p98 = force_token()
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        i92 = int_add(i84, 100)
        i93 = int_add(i84, 1)
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i73 = int_le(i70, 0),
        guard_false(i73, descr=<Guard0x9c13130>),
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

    def test_block_passing(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        1 to: 100000 do: [:i | [:blk | blk value: i] value: [:x | x + 100]].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x12a02f10>)
        i91 = int_le(i84, 100000)
        guard_true(i91, descr=<Guard0x12a02ee0>)
        p98 = force_token()
        enter_portal_frame(0, 0)
        p99 = force_token()
        enter_portal_frame(0, 0)
        i92 = int_add(i84, 100)
        leave_portal_frame(0)
        leave_portal_frame(0)
        i93 = int_add(i84, 1)
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i73 = int_le(i70, 0),
        guard_false(i73, descr=<Guard0x9c13130>),
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, i93, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i94, p68, descr=TargetToken(312516328))
        """)

    @py.test.mark.skipif("'Not ready'")
    def test_collection_at(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | o |
        o := OrderedCollection newFrom: #(12 13 123 13 1 123 132 132 123 1 213 123 112  2).
        1 to: 100000 do: [:i | o at: 2].
        """)
        self.assert_matches(traces[0].loop, """
        """)

    @py.test.mark.skipif("'Not ready'")
    def test_collect(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | o |
        o := OrderedCollection newFrom: #(12 13 123 13 1 123 132 132 123 1 213 123 112  2).
        1 to: 100000 do: [:i | o collect: [:e | e * 2]].
        """)
        self.assert_matches(traces[0].loop, """
        """)

    @py.test.mark.skipif("'Not ready'")
    def test_inject(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | o |
        o := OrderedCollection newFrom: #(12 13 123 13 1 123 132 132 123 1 213 123 112  2).
        1 to: 100000 do: [:i | o inject: 0 into: [:sum :e | e + sum]].
        """)
        self.assert_matches(traces[0].loop, """
        """)

    def test_mixed_stack(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | a |
        a := 0.
        (1 to: 10000) do: [:i|
          a := ((1 to: 10000) do: [:j| j + i]) last bitOr: a
        ].
        """)
        self.assert_matches(traces[0].loop,
        """
        guard_not_invalidated(descr=<Guard0x45fa708>)
        i83 = int_lt(i59, i49)
        guard_true(i83, descr=<Guard0x60ec848>)
        i84 = int_mul_ovf(i59, i55)
        guard_no_overflow(descr=<Guard0x60ec800>)
        i85 = int_add_ovf(i54, i84)
        guard_no_overflow(descr=<Guard0x60ec7b8>)
        i87 = int_add(i59, 1)
        p88 = force_token()
        enter_portal_frame(1, 0)
        i91 = int_add_ovf(i85, i73)
        guard_no_overflow(descr=<Guard0x60ec728>)
        leave_portal_frame(1)
        i94 = int_sub(i79, 1)
        setfield_gc(ConstPtr(ptr95), i94, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i97 = int_le(i94, 0)
        guard_false(i97, descr=<Guard0x45fa6a0>)
        i99 = arraylen_gc(p66, descr=<ArrayP 8>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i85, i87, p17, i91, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i49, i55, i54, p66, p68, i73, i94, descr=TargetToken(100984816))
        """)

    def test_global_class_access(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | a |
        a := nil.
        (1 to: 1000000) do: [:i|
           a := OrderedCollection compilerClass.
        ].
        """)
        self.assert_matches(traces[0].loop,
        """
        guard_not_invalidated(descr=<Guard0x52eac50>)
        i112 = int_lt(i59, i49)
        guard_true(i112, descr=<Guard0x6ddc608>)
        i113 = int_mul_ovf(i59, i55)
        guard_no_overflow(descr=<Guard0x6ddc5c0>)
        i114 = int_add_ovf(i54, i113)
        guard_no_overflow(descr=<Guard0x6ddc578>)
        i116 = int_add(i59, 1)
        p118 = getarrayitem_gc_r(p66, 0, descr=<ArrayP 8>)
        guard_nonnull_class(p118, 13692960, descr=<Guard0x52eabe8>)
        p120 = force_token()
        enter_portal_frame(1, 0)
        p123 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        p127 = getfield_gc_r(p118, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p127, ConstPtr(ptr128), descr=<Guard0x52eab80>)
        p129 = getfield_gc_r(p118, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 24>)
        leave_portal_frame(1)
        i132 = int_sub(i106, 1)
        setfield_gc(ConstPtr(ptr133), i132, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        setarrayitem_gc(p129, 0, p111, descr=<ArrayP 8>)
        i136 = int_le(i132, 0)
        guard_false(i136, descr=<Guard0x52eab18>)
        i138 = arraylen_gc(p66, descr=<ArrayP 8>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i114, i116, p17, p111, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i49, i55, i54, p66, p77, p79, i132, p111, descr=TargetToken(114551088))
        """)

    @py.test.mark.skipif("'Flaky, check with pypy devs'")
    def test_benchFib(self, spy, tmpdir):
        """Tests how well call_assembler and int-local-return works"""
        traces = self.run(spy, tmpdir, """
        25 benchFib
        """)
        self.assert_matches(traces[0].bridges[-1], """
        guard_value(i7, 0, descr=<Guard0xa655430>)
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

    def test_make_float(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | a |
        1 to: 1000000 do: [:i|
           a := (Float basicNew: 2).
           a basicAt: 1 put: i.
           a basicAt: 2 put: i.
           a + 2.
        ].
        """)
        if IS_64BIT:
            self.assert_matches(traces[1].loop,
            """
            guard_not_invalidated(descr=<Guard0x41fa770>)
            i116 = int_le(i107, 1000000)
            guard_true(i116, descr=<Guard0x5d6ade8>)
            p117 = force_token()
            enter_portal_frame(1, 0)
            i121 = int_and(i107, 4294967295)
            leave_portal_frame(1)
            p123 = force_token()
            enter_portal_frame(1, 0)
            leave_portal_frame(1)
            i128 = int_lshift(i121, 32)
            i129 = int_or(i128, i121)
            i131 = uint_rshift(i129, 63)
            i133 = int_and(i129, 9218868437227405312)
            i135 = uint_rshift(i133, 52)
            i137 = int_and(i129, 4503599627370495)
            guard_value(i135, 0, descr=<Guard0x5d6ad58>)
            f140 = call_f(ConstClass(_ll_1_cast_uint_to_float__Unsigned), i137, descr=<Callf 8 i EF=2>)
            i142 = float_eq(f140, 0.000000)
            guard_false(i142, descr=<Guard0x41fa708>)
            f144 = float_sub(f140, f140)
            i146 = float_eq(f144, 0.000000)
            guard_true(i146, descr=<Guard0x5d6ad10>)
            f149 = call_f(ConstClass(ccall_ldexp), f140, -1074, descr=<Callf 8 fi EF=2>)
            i152 = call_i(ConstClass(_ll_1_threadlocalref_get__INTLlT_Signed), 48, descr=<Calli 4 i EF=2 OS=5>)
            f154 = float_add(f149, 11235582092889474423308157442431404585112356118389416079589380072358292237843810195794279832650471001320007117491962084853674360550901038905802964414967132773610493339054092829768888725077880882465817684505312860552384417646403930092119569408801702322709406917786643639996702871154982269052209770601514008576.000000)
            i155 = float_eq(f154, f149)
            guard_false(i155, descr=<Guard0x41fa6a0>)
            i156 = int_is_true(i152)
            guard_false(i156, descr=<Guard0x5d6acc8>)
            i157 = int_is_true(i131)
            guard_false(i157, descr=<Guard0x5d6ac80>)
            f159 = float_add(f149, 2.000000)
            i161 = int_add(i107, 1)
            i163 = int_sub(i111, 1)
            setfield_gc(ConstPtr(ptr164), i163, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i166 = int_le(i163, 0)
            guard_false(i166, descr=<Guard0x41fa638>)
            jump(p0, p1, i2, p4, p5, p6, p8, i121, i161, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p52, i163, descr=TargetToken(97308768))
            """)
        else:
            self.assert_matches(traces[1].loop,
            """
            guard_not_invalidated(descr=<Guard0x94415f4>)
            i134 = int_le(i125, 1000000)
            guard_true(i134, descr=<Guard0x94d5ed4>)
            p135 = force_token()
            enter_portal_frame(4, 0)
            leave_portal_frame(4)
            p139 = force_token()
            enter_portal_frame(4, 0)
            leave_portal_frame(4)
            f144 = call_f(ConstClass(_ll_1_ullong_from_uint__Unsigned), i125, descr=<CallL 8 i EF=0 OS=93>)
            f147 = call_f(ConstClass(_ll_2_ullong_lshift__UnsignedLongLong_Signed), f144, 32, descr=<CallL 8 Li EF=0 OS=81>)
            f149 = call_f(ConstClass(_ll_2_ullong_or__UnsignedLongLong_UnsignedLongLong), f147, f144, descr=<CallL 8 LL EF=0 OS=80>)
            f152 = call_f(ConstClass(_ll_2_ullong_urshift__UnsignedLongLong_Signed), f149, 63, descr=<CallL 8 Li EF=0 OS=92>)
            i154 = call_i(ConstClass(_ll_1_llong_to_int__UnsignedLongLong), f152, descr=<Calli 4 L EF=0 OS=85>)
            f157 = call_f(ConstClass(_ll_2_ullong_and__UnsignedLongLong_UnsignedLongLong), f149, inf, descr=<CallL 8 LL EF=0 OS=79>)
            f160 = call_f(ConstClass(_ll_2_ullong_urshift__UnsignedLongLong_Signed), f157, 52, descr=<CallL 8 Li EF=0 OS=92>)
            i162 = call_i(ConstClass(_ll_1_llong_to_int__UnsignedLongLong), f160, descr=<Calli 4 L EF=0 OS=85>)
            f165 = call_f(ConstClass(_ll_2_ullong_and__UnsignedLongLong_UnsignedLongLong), f149, 0.000000, descr=<CallL 8 LL EF=0 OS=79>)
            guard_value(i162, 0, descr=<Guard0x9441628>)
            f169 = call_f(ConstClass(_ll_1_ullong_u_to_float__UnsignedLongLong), f165, descr=<Callf 8 L EF=0 OS=94>)
            i171 = float_eq(f169, 0.000000)
            guard_false(i171, descr=<Guard0x944165c>)
            f172 = float_sub(f169, f169)
            i174 = float_eq(f172, 0.000000)
            guard_true(i174, descr=<Guard0x94d5ef8>)
            f177 = call_f(ConstClass(ccall_ldexp), f169, -1074, descr=<Callf 8 fi EF=2>)
            i180 = call_i(ConstClass(_ll_1_threadlocalref_get__SignedLlT_Signed), 28, descr=<Calli 4 i EF=2 OS=5>)
            f182 = float_add(f177, 11235582092889474423308157442431404585112356118389416079589380072358292237843810195794279832650471001320007117491962084853674360550901038905802964414967132773610493339054092829768888725077880882465817684505312860552384417646403930092119569408801702322709406917786643639996702871154982269052209770601514008576.000000)
            i183 = float_eq(f182, f177)
            guard_false(i183, descr=<Guard0x9441690>)
            i184 = int_is_true(i180)
            guard_false(i184, descr=<Guard0x94d5f1c>)
            i185 = int_is_true(i154)
            guard_false(i185, descr=<Guard0x94d5f40>)
            f187 = float_add(f177, 2.000000)
            i189 = int_add(i125, 1)
            i191 = int_sub(i129, 1)
            setfield_gc(ConstPtr(ptr192), i191, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>)
            i194 = int_le(i191, 0)
            guard_false(i194, descr=<Guard0x94416c4>)
            jump(p0, p1, i2, p3, p4, p7, p8, p10, i125, i189, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p55, p57, i191, descr=TargetToken(155445900))
            """)

    def test_new_large_int(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | a |
        1 to: 1000000 do: [:i|
            a := Integer new: 8 neg: false.
            a := a + 2.
        ].
        """)
        if IS_64BIT:
            self.assert_matches(traces[0].loop,
            """
            guard_not_invalidated(descr=<Guard0x4513540>)
            i118 = int_le(i108, 1000000)
            guard_true(i118, descr=<Guard0x6002f08>)
            p119 = force_token()
            enter_portal_frame(1, 0)
            p122 = force_token()
            enter_portal_frame(1, 0)
            leave_portal_frame(1)
            leave_portal_frame(1)
            p128 = new_array_clear(8, descr=<ArrayU 1>)
            p132 = call_r(ConstClass(frombytes), p128, ConstPtr(ptr130), 0, descr=<Callr 8 rri EF=4>)
            guard_no_exception(descr=<Guard0x45134d8>)
            p135 = call_r(ConstClass(rbigint.add), p132, ConstPtr(ptr134), descr=<Callr 8 rr EF=4>)
            guard_no_exception(descr=<Guard0x4513470>)
            i136 = getfield_gc_i(p135, descr=<FieldS rpython.rlib.rbigint.rbigint.inst_sign 16 pure>)
            i138 = int_ge(i136, 0)
            guard_true(i138, descr=<Guard0x6002ec0>)
            i139 = getfield_gc_i(p135, descr=<FieldS rpython.rlib.rbigint.rbigint.inst_size 24 pure>)
            i141 = int_le(i139, 2)
            guard_true(i141, descr=<Guard0x6002de8>)
            i143 = call_i(ConstClass(rbigint._touint_helper), p135, descr=<Calli 8 r EF=4>)
            guard_no_exception(descr=<Guard0x4513408>)
            i145 = int_ge(i143, 0)
            guard_true(i145, descr=<Guard0x6002da0>)
            i147 = int_add(i108, 1)
            i149 = int_sub(i112, 1)
            setfield_gc(ConstPtr(ptr150), i149, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i152 = int_le(i149, 0)
            guard_false(i152, descr=<Guard0x45133a0>)
            jump(p0, p1, i2, p4, p5, p6, p8, i143, i147, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p52, p54, p68, i149, descr=TargetToken(100027392))
            """)
        else:
            self.assert_matches(traces[0].loop,
            """
            guard_not_invalidated(descr=<Guard0x955a420>)
            i110 = int_le(i101, 1000000)
            guard_true(i110, descr=<Guard0x95c1ef8>)
            p111 = force_token()
            enter_portal_frame(4, 0)
            p114 = force_token()
            enter_portal_frame(4, 0)
            leave_portal_frame(4)
            leave_portal_frame(4)
            i120 = int_add(i101, 1)
            i122 = int_sub(i105, 1)
            setfield_gc(ConstPtr(ptr123), i122, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>)
            i125 = int_le(i122, 0)
            guard_false(i125, descr=<Guard0x955a454>)
            jump(p0, p1, i2, p3, p4, p7, p8, p10, i120, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p55, p57, p59, p76, p78, i122, descr=TargetToken(156580140))
            """)

    def test_large_negation0(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | a |
        a := 1 - (2 raisedTo: 128).
        1 to: 1000000 do: [:i|
            a negated.
        ].
        """)
        self.assert_matches(traces[0].loop,
        """
        guard_not_invalidated(descr=<Guard0x39f7748>)
        i121 = int_le(i112, 1000000)
        guard_true(i121, descr=<Guard0x54e6800>)
        p122 = getfield_gc_r(p11, descr=<FieldP rsqueakvm.model.base.W_AbstractObjectWithClassReference.inst_w_class 24>)
        guard_value(p122, ConstPtr(ptr123), descr=<Guard0x54e67b8>)
        p125 = getfield_gc_r(ConstPtr(ptr124), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p125, ConstPtr(ptr126), descr=<Guard0x39f76e0>)
        p127 = force_token()
        enter_portal_frame(1, 0)
        p131 = getfield_gc_r(ConstPtr(ptr130), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p131, ConstPtr(ptr132), descr=<Guard0x39f7678>)
        p134 = getfield_gc_r(ConstPtr(ptr133), descr=<FieldP rsqueakvm.model.pointers.W_FixedPointersObject.inst__field2 48>)
        guard_nonnull_class(p134, ConstClass(W_FixedPointersObject), descr=<Guard0x39f7610>)
        i136 = getfield_gc_i(p11, descr=<FieldS rsqueakvm.model.numeric.W_LargeIntegerBig.inst__exposed_size 32>)
        i138 = cond_call_value_i(i136, ConstClass(calculate_exposed_size_for_big_int), p64, descr=<Calli 8 r EF=4>)
        guard_no_exception(descr=<Guard0x39f75a8>)
        p140 = getfield_gc_r(ConstPtr(ptr139), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        setfield_gc(p11, i138, descr=<FieldS rsqueakvm.model.numeric.W_LargeIntegerBig.inst__exposed_size 32>)
        guard_value(p140, ConstPtr(ptr141), descr=<Guard0x39f7540>)
        p142 = getfield_gc_r(p134, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p142, ConstPtr(ptr143), descr=<Guard0x39f74d8>)
        p144 = getfield_gc_r(p142, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        guard_value(p144, ConstPtr(ptr145), descr=<Guard0x54e6728>)
        p147 = getfield_gc_r(ConstPtr(ptr146), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p147, ConstPtr(ptr148), descr=<Guard0x39f7470>)
        p149 = force_token()
        enter_portal_frame(1, 0)
        i153 = int_lt(i138, 0)
        guard_false(i153, descr=<Guard0x54e66e0>)
        p154 = new_array_clear(i138, descr=<ArrayU 1>)
        p156 = getfield_gc_r(ConstPtr(ptr155), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p156, ConstPtr(ptr157), descr=<Guard0x39f7408>)
        leave_portal_frame(1)
        p159 = force_token()
        enter_portal_frame(1, 0)
        p163 = getfield_gc_r(ConstPtr(ptr162), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p163, ConstPtr(ptr164), descr=<Guard0x39f73a0>)
        p165 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        i170 = int_sub(i138, 1)
        i171 = int_le(i138, i170)
        guard_false(i171, descr=<Guard0x54e6698>)
        p172 = force_token()
        p173 = new_with_vtable(descr=<SizeDescr 56>)
        p174 = new_with_vtable(descr=<SizeDescr 8>)
        setfield_gc(p173, ConstPtr(ptr175), descr=<FieldP rsqueakvm.model.base.W_AbstractObjectWithClassReference.inst_w_class 24>)
        setfield_gc(p0, p172, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        setfield_gc(p173, 0, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8 pure>)
        setfield_gc(p173, p154, descr=<FieldP rsqueakvm.model.variable.W_BytesObject.inst_bytes 32>)
        setfield_gc(p173, p174, descr=<FieldP rsqueakvm.model.variable.W_BytesObject.inst_version 40 pure>)
        call_may_force_n(ConstClass(_replace_from_to_trampoline__v133___simple_call__function__), 0, i170, 0, p173, p11, descr=<Callv 0 iiirr EF=7>)
        guard_not_forced(descr=<Guard0x8c2d2f0>)
        guard_no_exception(descr=<Guard0x54e6650>)
        guard_not_invalidated(descr=<Guard0x54e6608>)
        leave_portal_frame(1)
        leave_portal_frame(1)
        i183 = int_add(i112, 1)
        i185 = getfield_gc_i(ConstPtr(ptr184), descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i187 = int_sub(i185, 1)
        setfield_gc(ConstPtr(ptr188), i187, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i190 = int_le(i187, 0)
        guard_false(i190, descr=<Guard0x39f7268>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i183, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p64, descr=TargetToken(88378128))
        """)

    # This is a bit full, unfortunately, but there shouldn't be any BlockClosure accesses in this loop
    def test_interval_do(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | a b |
        a := 1 - (2 raisedTo: 128).
        (1 to: 1000000) do: [:i|
            b := a negated.
        ].
        """)
        self.assert_matches(traces[0].loop,
        """
        guard_not_invalidated(descr=<Guard0x4cdc2f8>)
        i153 = int_lt(i59, i49)
        guard_true(i153, descr=<Guard0x684d5c8>)
        p154 = getfield_gc_r(p6, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p154, ConstPtr(ptr155), descr=<Guard0x4cdc3c8>)
        i156 = getfield_gc_i(p6, descr=<FieldS rsqueakvm.model.pointers.W_FixedPointersObject.inst__intField1 56>)
        i157 = getfield_gc_i(p6, descr=<FieldS rsqueakvm.model.pointers.W_FixedPointersObject.inst__intField3 72>)
        i158 = int_mul_ovf(i59, i157)
        guard_no_overflow(descr=<Guard0x684d580>)
        i159 = int_add_ovf(i156, i158)
        guard_no_overflow(descr=<Guard0x684d538>)
        i161 = int_add(i59, 1)
        p163 = getarrayitem_gc_r(p66, 0, descr=<ArrayP 8>)
        guard_nonnull_class(p163, ConstClass(W_LargeIntegerBig), descr=<Guard0x4cdc360>)
        p166 = getarrayitem_gc_r(p66, 1, descr=<ArrayP 8>)
        guard_nonnull_class(p166, 13692960, descr=<Guard0x4cdc430>)
        p168 = force_token()
        enter_portal_frame(1, 0)
        p171 = getfield_gc_r(p163, descr=<FieldP rsqueakvm.model.base.W_AbstractObjectWithClassReference.inst_w_class 24>)
        guard_value(p171, ConstPtr(ptr172), descr=<Guard0x684d4a8>)
        p174 = getfield_gc_r(ConstPtr(ptr173), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p174, ConstPtr(ptr175), descr=<Guard0x4cdc290>)
        p176 = force_token()
        enter_portal_frame(1, 0)
        p180 = getfield_gc_r(ConstPtr(ptr179), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p180, ConstPtr(ptr181), descr=<Guard0x4cdc228>)
        p183 = getfield_gc_r(ConstPtr(ptr182), descr=<FieldP rsqueakvm.model.pointers.W_FixedPointersObject.inst__field2 48>)
        guard_nonnull_class(p183, ConstClass(W_FixedPointersObject), descr=<Guard0x4cdc1c0>)
        i185 = getfield_gc_i(p163, descr=<FieldS rsqueakvm.model.numeric.W_LargeIntegerBig.inst__exposed_size 32>)
        p186 = getfield_gc_r(p163, descr=<FieldP rsqueakvm.model.numeric.W_LargeIntegerBig.inst_value 40 pure>)
        i188 = cond_call_value_i(i185, ConstClass(calculate_exposed_size_for_big_int), p186, descr=<Calli 8 r EF=4>)
        guard_no_exception(descr=<Guard0x4cdc158>)
        p190 = getfield_gc_r(ConstPtr(ptr189), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        setfield_gc(p163, i188, descr=<FieldS rsqueakvm.model.numeric.W_LargeIntegerBig.inst__exposed_size 32>)
        guard_value(p190, ConstPtr(ptr191), descr=<Guard0x4cdc0f0>)
        p192 = getfield_gc_r(p183, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p192, ConstPtr(ptr193), descr=<Guard0x4cdc088>)
        p194 = getfield_gc_r(p192, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        guard_value(p194, ConstPtr(ptr195), descr=<Guard0x684d418>)
        p197 = getfield_gc_r(ConstPtr(ptr196), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p197, ConstPtr(ptr198), descr=<Guard0x4cdc020>)
        p199 = force_token()
        enter_portal_frame(1, 0)
        i203 = int_lt(i188, 0)
        guard_false(i203, descr=<Guard0x684d3d0>)
        p204 = new_array_clear(i188, descr=<ArrayU 1>)
        p206 = getfield_gc_r(ConstPtr(ptr205), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p206, ConstPtr(ptr207), descr=<Guard0x4d5bf00>)
        leave_portal_frame(1)
        p209 = force_token()
        enter_portal_frame(1, 0)
        p213 = getfield_gc_r(ConstPtr(ptr212), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p213, ConstPtr(ptr214), descr=<Guard0x4cdc498>)
        p215 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        i220 = int_sub(i188, 1)
        i221 = int_le(i188, i220)
        guard_false(i221, descr=<Guard0x684d610>)
        p222 = force_token()
        p223 = new_with_vtable(descr=<SizeDescr 56>)
        p224 = new_with_vtable(descr=<SizeDescr 8>)
        setfield_gc(p223, p224, descr=<FieldP rsqueakvm.model.variable.W_BytesObject.inst_version 40 pure>)
        setfield_gc(p223, 0, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8 pure>)
        setfield_gc(p223, ConstPtr(ptr226), descr=<FieldP rsqueakvm.model.base.W_AbstractObjectWithClassReference.inst_w_class 24>)
        setfield_gc(p0, p222, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        setfield_gc(p223, p204, descr=<FieldP rsqueakvm.model.variable.W_BytesObject.inst_bytes 32>)
        call_may_force_n(ConstClass(_replace_from_to_trampoline__v133___simple_call__function__), 0, i220, 0, p223, p163, descr=<Callv 0 iiirr EF=7>)
        guard_not_forced(descr=<Guard0x9f8f280>)
        guard_no_exception(descr=<Guard0x684d658>)
        guard_not_invalidated(descr=<Guard0x684d6a0>)
        leave_portal_frame(1)
        leave_portal_frame(1)
        p232 = getfield_gc_r(p166, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p232, ConstPtr(ptr233), descr=<Guard0x4cdc568>)
        p235 = getfield_gc_r(ConstPtr(ptr234), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p235, ConstPtr(ptr236), descr=<Guard0x4cdc5d0>)
        p237 = getfield_gc_r(p166, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 24>)
        leave_portal_frame(1)
        i240 = getfield_gc_i(ConstPtr(ptr239), descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i242 = int_sub(i240, 1)
        setfield_gc(ConstPtr(ptr243), i242, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        setarrayitem_gc(p237, 0, p223, descr=<ArrayP 8>)
        i246 = int_le(i242, 0)
        guard_false(i246, descr=<Guard0x4cdc638>)
        i247 = arraylen_gc(p66, descr=<ArrayP 8>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i159, i161, p17, p223, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i49, p66, p64, descr=TargetToken(108721024))
        """)

    def test_dnu(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | c i |
        Object
            subclass: #MyA
            instanceVariableNames: ''
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := Smalltalk at: #MyA  .
        c compile: 'doesNotUnderstand: aMessage
        ^ {aMessage selector. aMessage arguments. aMessage lookupClass}' classified: 'none' withStamp: nil notifying: nil logSource: false.

        i := c new.
        1 to: 100000 do: [:ignored | i foo ]
        """)
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0x9facdf0>)
        i77 = int_le(i68, 100000)
        guard_true(i77, descr=<Guard0x9f9cb60>)
        p78 = force_token()
        enter_portal_frame(4, 0)
        leave_portal_frame(4)
        i83 = int_add(i68, 1)
        i85 = int_sub(i72, 1)
        setfield_gc(ConstPtr(ptr86), i85, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i88 = int_le(i85, 0)
        guard_false(i88, descr=<Guard0x9facd88>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, p13, i83, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i85, descr=TargetToken(166460720))
        """)

    def test_oam(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | c i |
        Object
            subclass: #MyOaM
            instanceVariableNames: ''
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := Smalltalk at: #MyOaM.
        c compile: 'run: aSelector with: someArgs in: aReceiver
        ^ {aSelector. someArgs. aReceiver}' classified: 'none' withStamp: nil notifying: nil logSource: false.

        Object
            subclass: #MyA
            instanceVariableNames: ''
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := Smalltalk at: #MyA.
        c methodDict at: #oam put: (Smalltalk at: #MyOaM) new.
        i := c new.
        1 to: 100000 do: [:ignored | i oam ]
        """)
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0xafaeff8>)
        i80 = int_le(i71, 100000)
        guard_true(i80, descr=<Guard0xaea95c8>)
        p81 = force_token()
        enter_portal_frame(4, 0)
        leave_portal_frame(4)
        i86 = int_add(i71, 1)
        i88 = int_sub(i75, 1)
        setfield_gc(ConstPtr(ptr89), i88, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i91 = int_le(i88, 0)
        guard_false(i91, descr=<Guard0xafaf060>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, p13, i86, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i88, descr=TargetToken(184206704))
        """)

    def test_literal_array(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | c i |
        Object
            subclass: #MyA
            instanceVariableNames: ''
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := Smalltalk at: #MyA.
        c compile: 'newLiteralArray
        ^ {$c. 1. 12.0. self}' classified: 'none' withStamp: nil notifying: nil logSource: false.
        i := c new.
        1 to: 100000 do: [:ignored | i newLiteralArray ]
        """)
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0xb605748>)
        i74 = int_le(i65, 100000)
        guard_true(i74, descr=<Guard0xb608608>)
        p75 = force_token()
        enter_portal_frame(4, 0)
        leave_portal_frame(4)
        i80 = int_add(i65, 1)
        i82 = int_sub(i69, 1)
        setfield_gc(ConstPtr(ptr83), i82, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i85 = int_le(i82, 0)
        guard_false(i85, descr=<Guard0xb6057b0>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, p13, i80, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i82, descr=TargetToken(189806080))
        """)

    def test_identity_hash_known_object(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | c |
        Object
            subclass: #MyA
            instanceVariableNames: ''
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := (Smalltalk at: #MyA) new.
        1 to: 10000 do: [:i | c identityHash ].
        """)
        # important: there shouldn't be a getfield inst_hash before the cond_call to calculate the hash
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0x36c5880>)
        i67 = int_le(i58, 10000)
        guard_true(i67, descr=<Guard0x3de4ec0>)
        i69 = cond_call_value_i(i54, ConstClass(calculate_and_cache), p11, descr=<Calli 8 r EF=5>)
        guard_no_exception(descr=<Guard0x36c58e8>)
        guard_not_invalidated(descr=<Guard0x3de4f08>)
        i71 = int_add(i58, 1)
        i73 = int_sub(i62, 1)
        setfield_gc(ConstPtr(ptr74), i73, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i76 = int_le(i73, 0)
        guard_false(i76, descr=<Guard0x36c5950>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i71, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i54, i73, descr=TargetToken(86285936))
        """)

    def test_identity_hash_fresh_object(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | c coll s |
        Object
            subclass: #MyA
            instanceVariableNames: ''
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := Smalltalk at: #MyA.
        coll := OrderedCollection new.
        1 to: 10000 do: [:ignored | coll add: c new].
        1 to: 10000 do: [:i | (coll at: i) identityHash ].
        """)
        # important: there shouldn't be a setfield inst_hash or an abort, just a getfield for the hash value and a cond_call
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0x42fb880>)
        i103 = int_le(i94, 10000)
        guard_true(i103, descr=<Guard0x44464a0>)
        p104 = force_token()
        enter_portal_frame(1, 0)
        i107 = int_add_ovf(i94, i62)
        guard_no_overflow(descr=<Guard0x44464e8>)
        i109 = int_sub(i107, 1)
        i110 = int_gt(i109, i66)
        guard_false(i110, descr=<Guard0x4446530>)
        i112 = int_sub(i109, 1)
        i113 = uint_lt(i112, i78)
        guard_true(i113, descr=<Guard0x4446578>)
        i115 = int_lt(i112, 0)
        guard_false(i115, descr=<Guard0x44465c0>)
        p116 = getarrayitem_gc_r(p77, i112, descr=<ArrayP 8>)
        guard_nonnull_class(p116, ConstClass(W_FixedPointersObject), descr=<Guard0x42fb8e8>)
        leave_portal_frame(1)
        p119 = getfield_gc_r(p116, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p119, ConstPtr(ptr120), descr=<Guard0x42fb950>)
        i121 = getfield_gc_i(p116, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8 pure>)
        i123 = cond_call_value_i(i121, ConstClass(calculate_and_cache), p116, descr=<Calli 8 r EF=5>)
        guard_no_exception(descr=<Guard0x42fb9b8>)
        guard_not_invalidated(descr=<Guard0x4446608>)
        i125 = int_add(i94, 1)
        i127 = int_sub(i98, 1)
        setfield_gc(ConstPtr(ptr128), i127, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i130 = int_le(i127, 0)
        guard_false(i130, descr=<Guard0x42fba88>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, p13, p15, p17, i125, p25, p27, p29, p31, p33, p35, p37, p39, p41, i62, i66, p68, i78, p77, i127, descr=TargetToken(78064480))
        """)

    def test_nested_closure_loop_call(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        1 to: 2000 do: [:a |
            (1 to: 1000) do: [:b |
                b abs.
            ]
        ].
        """)
        # important: there should be two loops, with the outer calling the inner, and in between an entry bridge to the inner loop abs call
        self.assert_matches(traces[-3].loop, """
        guard_not_invalidated(descr=<Guard0x4c38c50>)
        i99 = int_lt(i59, i49)
        guard_true(i99, descr=<Guard0x52dbb68>)
        i100 = int_mul_ovf(i59, i55)
        guard_no_overflow(descr=<Guard0x52dbb20>)
        i101 = int_add_ovf(i54, i100)
        guard_no_overflow(descr=<Guard0x52dbad8>)
        i103 = int_add(i59, 1)
        p104 = force_token()
        enter_portal_frame(1, 0)
        p107 = force_token()
        enter_portal_frame(1, 0)
        i111 = int_lt(i101, 0)
        guard_false(i111, descr=<Guard0x52dba48>)
        leave_portal_frame(1)
        leave_portal_frame(1)
        i115 = int_sub(i95, 1)
        setfield_gc(ConstPtr(ptr116), i115, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i118 = int_le(i115, 0)
        guard_false(i118, descr=<Guard0x4c39130>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i101, i103, p17, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i49, i55, i54, i115, descr=TargetToken(108261712))
        """)
        assert len(traces[-2].setup) > 0
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0x4894ec0>)
        i184 = int_le(i175, 2000)
        guard_true(i184, descr=<Guard0x6403cd0>)
        p186 = getfield_gc_r(ConstPtr(ptr185), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p186, ConstPtr(ptr187), descr=<Guard0x4895060>)
        p188 = force_token()
        enter_portal_frame(1, 0)
        p192 = getfield_gc_r(ConstPtr(ptr191), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p192, ConstPtr(ptr193), descr=<Guard0x4894ff8>)
        p195 = getfield_gc_r(ConstPtr(ptr194), descr=<FieldP rsqueakvm.model.pointers.W_FixedPointersObject.inst__field2 48>)
        guard_nonnull_class(p195, ConstClass(W_FixedPointersObject), descr=<Guard0x4894f90>)
        p197 = getfield_gc_r(p195, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p197, ConstPtr(ptr198), descr=<Guard0x4894f28>)
        p199 = getfield_gc_r(p197, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        guard_value(p199, ConstPtr(ptr200), descr=<Guard0x6403c88>)
        p202 = getfield_gc_r(ConstPtr(ptr201), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p202, ConstPtr(ptr203), descr=<Guard0x4894e58>)
        p204 = force_token()
        enter_portal_frame(1, 0)
        p208 = getfield_gc_r(ConstPtr(ptr207), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p208, ConstPtr(ptr209), descr=<Guard0x4894df0>)
        p211 = getfield_gc_r(ConstPtr(ptr210), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_value(p211, ConstPtr(ptr212), descr=<Guard0x4894d88>)
        p213 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        leave_portal_frame(1)
        p219 = getfield_gc_r(p1, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        guard_nonnull_class(p219, ConstClass(ContextPartShadow), descr=<Guard0x4895130>)
        i221 = ptr_eq(p219, p0)
        guard_true(i221, descr=<Guard0x6403d18>)
        p222 = force_token()
        enter_portal_frame(1, 0)
        p225 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        p229 = force_token()
        enter_portal_frame(1, 0)
        p232 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        i238 = int_sub(i179, 1)
        setfield_gc(ConstPtr(ptr239), i238, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i241 = int_le(i238, 0)
        guard_false(i241, descr=<Guard0x4895198>)
        p242 = force_token()
        p243 = new_with_vtable(descr=<SizeDescr 96>)
        p244 = new_with_vtable(descr=<SizeDescr 24>)
        setfield_gc(p244, p222, descr=<FieldP JitVirtualRef.virtual_token 8>)
        setfield_gc(p244, ConstPtr(null), descr=<FieldP JitVirtualRef.forced 16>)
        p246 = new_array_clear(17, descr=<ArrayP 8>)
        p247 = new_with_vtable(descr=<SizeDescr 80>)
        setfield_gc(p247, 0, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8 pure>)
        setfield_gc(p247, ConstPtr(ptr249), descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__stack 24 pure>)
        setfield_gc(p247, 268435503, descr=<FieldU rsqueakvm.model.block_closure.W_BlockClosure.inst__startpc_stacklen_args 32 pure>)
        setfield_gc(p247, ConstPtr(ptr251), descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__w_method 40 pure>)
        setfield_gc(p247, p1, descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__w_outerContext 48 pure>)
        setfield_gc(p247, p6, descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__w_receiver 56 pure>)
        setfield_gc(p247, ConstPtr(ptr252), descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst_version 64 pure>)
        setarrayitem_gc(p246, 0, p247, descr=<ArrayP 8>)
        p254 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p254, 1, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p246, 1, p254, descr=<ArrayP 8>)
        p257 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p257, 1, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p246, 2, p257, descr=<ArrayP 8>)
        p260 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p260, 1000, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p246, 3, p260, descr=<ArrayP 8>)
        p263 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p263, 1, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p246, 4, p263, descr=<ArrayP 8>)
        setarrayitem_gc(p246, 5, ConstPtr(ptr267), descr=<ArrayP 8>)
        setarrayitem_gc(p246, 6, ConstPtr(ptr269), descr=<ArrayP 8>)
        p270 = new_with_vtable(descr=<SizeDescr 96>)
        setfield_gc(p270, 0, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8 pure>)
        setfield_gc(p270, ConstPtr(null), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 24>)
        setfield_gc(p270, ConstPtr(ptr273), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 32>)
        setfield_gc(p270, ConstPtr(null), descr=<FieldP rsqueakvm.model.pointers.W_FixedPointersObject.inst__field1 40>)
        setfield_gc(p270, ConstPtr(null), descr=<FieldP rsqueakvm.model.pointers.W_FixedPointersObject.inst__field2 48>)
        setfield_gc(p270, 1, descr=<FieldS rsqueakvm.model.pointers.W_FixedPointersObject.inst__intField1 56>)
        setfield_gc(p270, 1000, descr=<FieldS rsqueakvm.model.pointers.W_FixedPointersObject.inst__intField2 64>)
        setfield_gc(p270, 1, descr=<FieldS rsqueakvm.model.pointers.W_FixedPointersObject.inst__intField3 72>)
        setfield_gc(p270, ConstPtr(null), descr=<FieldP rsqueakvm.model.pointers.W_FixedPointersObject.inst__intFields 80>)
        setfield_gc(p270, 3, descr=<FieldS rsqueakvm.model.pointers.W_FixedPointersObject.inst__size 88>)
        setfield_gc(p243, ConstPtr(ptr281), descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        setfield_gc(p0, p242, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        setfield_gc(p243, p244, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>)
        setfield_gc(p243, 1090519045, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        setfield_gc(p243, p246, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__temps_and_stack 40>)
        setfield_gc(p243, ConstPtr(ptr283), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 48>)
        setfield_gc(p243, p270, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_receiver 56>)
        setfield_gc(p243, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self 64>)
        setfield_gc(p243, 23, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self_size 72>)
        setfield_gc(p243, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 80>)
        setfield_gc(p243, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_extra_data 88>)
        call_assembler_n(p243, descr=<Loop2>)
        guard_not_forced(descr=<Guard0x9b49590>)
        keepalive(p243)
        p289 = guard_exception(13757968, descr=<Guard0x4895200>)
        i290 = ptr_eq(p243, p0)
        guard_false(i290, descr=<Guard0x4895268>)
        p291 = getfield_gc_r(p243, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        i293 = ptr_ne(p291, ConstPtr(null))
        cond_call(i293, 6399472, p243, descr=<Callv 0 r EF=2 OS=121>)
        i295 = getfield_gc_i(p243, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        guard_value(i295, 1090519067, descr=<Guard0x48952d0>)
        guard_not_invalidated(descr=<Guard0x6403d60>)
        p297 = getfield_gc_r(p243, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 80>)
        guard_isnull(p297, descr=<Guard0x6403da8>)
        p298 = getfield_gc_r(p243, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 48>)
        guard_value(p298, ConstPtr(ptr299), descr=<Guard0x4895338>)
        setfield_gc(p243, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>)
        setfield_gc(p243, 1094713343, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        guard_class(p289, 13757968, descr=<Guard0x48953a0>)
        p303 = getfield_gc_r(p289, descr=<FieldP rsqueakvm.interpreter.WrappedLocalReturn.inst_w_value 8 pure>)
        setfield_gc(p243, 20971519, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        setfield_gc(p244, ConstPtr(null), descr=<FieldP JitVirtualRef.virtual_token 8>)
        guard_nonnull(p303, descr=<Guard0x4895408>)
        i306 = int_add(i175, 1)
        i308 = getfield_gc_i(ConstPtr(ptr307), descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i310 = int_sub(i308, 1)
        setfield_gc(ConstPtr(ptr311), i310, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i313 = int_le(i310, 0)
        guard_false(i313, descr=<Guard0x4895470>)
        jump(p0, p1, i2, p4, p5, p6, p8, i306, p17, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i310, descr=TargetToken(104224336))
        """)

    def test_object_access_with_integer(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | o |
        o := OrderedCollection new.
        o add: 'foo'; add: 'bar'.
        1 to: 10000 do: [:i | o first].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x3ee7408>)
        i96 = int_le(i87, 10000)
        guard_true(i96, descr=<Guard0x59d8fe0>)
        p97 = force_token()
        enter_portal_frame(1, 0)
        p100 = force_token()
        enter_portal_frame(1, 0)
        p103 = getarrayitem_gc_r(p77, i76, descr=<ArrayP 8>)
        guard_nonnull_class(p103, ConstClass(W_BytesObject), descr=<Guard0x3ee7470>)
        leave_portal_frame(1)
        leave_portal_frame(1)
        i108 = int_add(i87, 1)
        i110 = int_sub(i91, 1)
        setfield_gc(ConstPtr(ptr111), i110, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i113 = int_le(i110, 0)
        guard_false(i113, descr=<Guard0x3ee74d8>)
        i115 = int_add(1, i63)
        i116 = int_sub(i63, 1)
        i117 = uint_lt(i116, i78)
        guard_true(i117, descr=<Guard0x3ee7540>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i108, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p68, p77, i116, i110, i63, i78, descr=TargetToken(93564304))
        """)
