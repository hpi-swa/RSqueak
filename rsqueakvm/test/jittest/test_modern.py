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
        guard_not_invalidated(descr=<Guard0xa4a48a8>)
        i78 = int_lt(i61, 10000)
        guard_true(i78, descr=<Guard0xa485c40>)
        i80 = int_add(i61, 1)
        p81 = force_token()
        enter_portal_frame(4, 0)
        leave_portal_frame(4)
        i86 = int_sub(i72, 1)
        setfield_gc(ConstPtr(ptr87), i86, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        setarrayitem_gc(p53, 0, i80, descr=<ArrayS 8>)
        i90 = int_le(i86, 0)
        guard_false(i90, descr=<Guard0xa4a4f90>)
        i91 = arraylen_gc(p53, descr=<ArrayS 8>)
        jump(p0, p1, i2, p3, p4, p7, p8, p10, p13, p15, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p53, i80, i86, descr=TargetToken(170868656))
        """)

    def test_named_access(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | m |
        m := Morph new.
        1 to: 100000 do: [:i | m bounds ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xdb8da34>)
        i69 = int_le(i69, 100000)
        guard_true(i69, descr=<Guard0xdc9c7f0>)
        i71 = int_add(i69, 1)
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i73 = int_le(i70, 0),
        guard_false(i73, descr=<Guard0x9c13130>),
        i72 = arraylen_gc(p65, descr=<ArrayP 4>)
        jump(p0, p1, i2, p3, p6, p7, i8, i9, p10, p11, i13, p14, p17, i71, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p45, p47, p65, descr=TargetToken(231309508))
        """)

    def test_named_access_in_array(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | o |
        o := Array with: Morph new.
        1 to: 100000 do: [:i | (o at: 1) bounds ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xdb8da34>)
        i70 = int_le(i69, 100000)
        guard_true(i70, descr=<Guard0xdc9c7f0>)
        i71 = int_add(i69, 1)
        i73 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i74 = int_le(i73, 0),
        guard_false(i74, descr=<Guard0x9c13130>),
        i72 = arraylen_gc(p65, descr=<ArrayP 4>)
        jump(p0, p1, i2, p3, p6, p7, i8, i9, p10, p11, i13, p14, p17, i71, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p45, p47, p65, descr=TargetToken(231309508))
        """)

    @py.test.mark.skipif("'Not ready'")
    def test_named_access_in_do_block(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
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

    def test_named_access_fresh(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        1 to: 100000 do: [:i | Morph new bounds ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xe445c70>)
        i169 = int_le(i162, 100000)
        guard_true(i169, descr=<Guard0xe5ec7b8>)
        p98 = force_token()
        enter_portal_frame(0, 0)
        p99 = force_token()
        enter_portal_frame(0, 0)
        p100 = force_token()
        enter_portal_frame(0, 0)
        p101 = force_token()
        enter_portal_frame(0, 0)
        p102 = force_token()
        enter_portal_frame(0, 0)
        p103 = force_token()
        enter_portal_frame(0, 0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        leave_portal_frame(0)
        p104 = force_token()
        enter_portal_frame(0, 0)
        p105 = force_token()
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

    def test_named_access_and_send(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | m |
        m := Morph new.
        1 to: 100000 do: [:i | m bounds outsetBy: 10 ].
        """)
        self.assert_matches(traces[2].loop, """
        guard_not_invalidated(descr=<Guard0xdba73ac>),
        i234 = int_le(i227, 100000)
        guard_true(i234, descr=<Guard0xdc6f080>)
        p98 = force_token()
        enter_portal_frame(4, 0)
        p99 = force_token()
        enter_portal_frame(4, 0)
        p100 = force_token()
        enter_portal_frame(4, 0)
        p101 = force_token()
        enter_portal_frame(4, 0)
        leave_portal_frame(4)
        leave_portal_frame(4)
        leave_portal_frame(4)
        p102 = force_token()
        enter_portal_frame(4, 0)
        p103 = force_token()
        enter_portal_frame(4, 0)
        p104 = force_token()
        enter_portal_frame(4, 0)
        leave_portal_frame(4)
        leave_portal_frame(4)
        leave_portal_frame(4)
        p105 = force_token()
        enter_portal_frame(4, 0)
        p106 = force_token()
        enter_portal_frame(4, 0)
        leave_portal_frame(4)
        leave_portal_frame(4)
        leave_portal_frame(4)
        i235 = int_add(i227, 1)
        i236 = int_sub(i231, 3)
        setfield_gc(ConstPtr(ptr228), i236, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 20>)
        i237 = int_le(i236, 0)
        guard_false(i237, descr=<Guard0xdba7380>)
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
        guard_not_invalidated(descr=<Guard0x5608cf1a9540>)
        i98 = int_lt(i70, i55)
        guard_true(i98, descr=<Guard0x5608ceb5b6a0>)
        i99 = int_mul_ovf(i70, i64)
        guard_no_overflow(descr=<Guard0x5608ceb5b658>)
        i100 = int_add_ovf(i60, i99)
        guard_no_overflow(descr=<Guard0x5608ceb5b5c8>)
        i102 = int_add(i70, 1)
        p103 = force_token()
        enter_portal_frame(1, 0)
        i106 = int_add_ovf(i100, i88)
        guard_no_overflow(descr=<Guard0x5608cf528020>)
        leave_portal_frame(1)
        i109 = int_sub(i94, 1)
        setfield_gc(ConstPtr(ptr110), i109, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i112 = int_le(i109, 0)
        guard_false(i112, descr=<Guard0x5608cf1a95a8>)
        i114 = arraylen_gc(p58, descr=<ArrayS 8>)
        i115 = arraylen_gc(p77, descr=<ArrayP 8>)
        jump(p0, p1, i2, p4, p6, p7, p9, p12, i100, i102, p18, i106, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, i55, p58, i64, i60, p77, p75, p83, p79, i88, i109, descr=TargetToken(94595864608128))
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
        guard_not_invalidated(descr=<Guard0x55b7b70cb198>)
        i131 = int_lt(i70, i55)
        guard_true(i131, descr=<Guard0x55b7b6a7cc38>)
        i132 = int_mul_ovf(i70, i64)
        guard_no_overflow(descr=<Guard0x55b7b6a7ccc8>)
        i133 = int_add_ovf(i60, i132)
        guard_no_overflow(descr=<Guard0x55b7b6a7cd58>)
        i135 = int_add(i70, 1)
        p137 = getarrayitem_gc_r(p77, 0, descr=<ArrayP 8>)
        guard_nonnull_class(p137, 94247479962480, descr=<Guard0x55b7b70cb200>)
        p139 = force_token()
        enter_portal_frame(1, 0)
        p142 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        p146 = getfield_gc_r(p137, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p146, ConstPtr(ptr147), descr=<Guard0x55b7b70cb268>)
        p148 = getfield_gc_r(p137, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 16>)
        leave_portal_frame(1)
        i151 = int_sub(i125, 1)
        setfield_gc(ConstPtr(ptr152), i151, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        setarrayitem_gc(p148, 0, p130, descr=<ArrayP 8>)
        i155 = int_le(i151, 0)
        guard_false(i155, descr=<Guard0x55b7b70cb2d0>)
        i157 = arraylen_gc(p58, descr=<ArrayS 8>)
        i158 = arraylen_gc(p77, descr=<ArrayP 8>)
        i159 = arraylen_gc(p92, descr=<ArrayP 8>)
        i160 = arraylen_gc(p110, descr=<ArrayP 8>)
        jump(p0, p1, i2, p4, p6, p7, p9, p12, i133, i135, p18, p130, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, i55, p58, i64, i60, p77, p75, p83, p92, p94, p96, p110, i151, p130, descr=TargetToken(94247568691776))
        """)

    @py.test.mark.skipif("'Flaky, check with pypy devs'")
    def test_benchFib(self, spy, tmpdir):
        """Tests how well call_assembler and int-local-return works"""
        traces = self.run(spy, tmpdir, """
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
            guard_not_invalidated(descr=<Guard0xa745f00>)
            i124 = int_le(i115, 1000000)
            guard_true(i124, descr=<Guard0xa75d148>)
            p125 = force_token()
            enter_portal_frame(4, 0)
            i129 = int_and(i115, 4294967295)
            leave_portal_frame(4)
            p131 = force_token()
            enter_portal_frame(4, 0)
            leave_portal_frame(4)
            i136 = int_lshift(i129, 32)
            i137 = int_or(i136, i129)
            i139 = uint_rshift(i137, 63)
            i141 = int_and(i137, 9218868437227405312)
            i143 = uint_rshift(i141, 52)
            i145 = int_and(i137, 4503599627370495)
            guard_value(i143, 0, descr=<Guard0xa75d190>)
            f148 = call_f(ConstClass(_ll_1_cast_uint_to_float__Unsigned), i145, descr=<Callf 8 i EF=2>)
            i150 = float_eq(f148, 0.000000)
            guard_false(i150, descr=<Guard0xa745f68>)
            f152 = float_sub(f148, f148)
            i154 = float_eq(f152, 0.000000)
            guard_true(i154, descr=<Guard0xa75d1d8>)
            f157 = call_f(ConstClass(ccall_ldexp), f148, -1074, descr=<Callf 8 fi EF=2>)
            i160 = call_i(ConstClass(_ll_1_threadlocalref_get__INTLlT_Signed), 48, descr=<Calli 4 i EF=2 OS=5>)
            f162 = float_add(f157, 11235582092889474423308157442431404585112356118389416079589380072358292237843810195794279832650471001320007117491962084853674360550901038905802964414967132773610493339054092829768888725077880882465817684505312860552384417646403930092119569408801702322709406917786643639996702871154982269052209770601514008576.000000)
            i163 = float_eq(f162, f157)
            guard_false(i163, descr=<Guard0xa8d2020>)
            i164 = int_is_true(i160)
            guard_false(i164, descr=<Guard0xa75d220>)
            i165 = int_is_true(i139)
            guard_false(i165, descr=<Guard0xa75d268>)
            f167 = float_add(f157, 2.000000)
            i169 = int_add(i115, 1)
            i171 = int_sub(i119, 1)
            setfield_gc(ConstPtr(ptr172), i171, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i174 = int_le(i171, 0)
            guard_false(i174, descr=<Guard0xa8d2088>)
            i175 = arraylen_gc(p55, descr=<ArrayP 8>)
            jump(p0, p1, i2, p3, p4, p7, p8, p10, i129, i169, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p55, p57, i171, descr=TargetToken(175413136))
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
            i195 = arraylen_gc(p55, descr=<ArrayP 4>)
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
            guard_not_invalidated(descr=<Guard0xa8ca020>)
            i130 = int_le(i120, 1000000)
            guard_true(i130, descr=<Guard0xa8a8608>)
            p131 = force_token()
            enter_portal_frame(4, 0)
            p134 = force_token()
            enter_portal_frame(4, 0)
            leave_portal_frame(4)
            leave_portal_frame(4)
            p140 = new_array_clear(8, descr=<ArrayU 1>)
            p144 = call_r(ConstClass(frombytes), p140, ConstPtr(ptr142), 0, descr=<Callr 8 rri EF=4>)
            guard_no_exception(descr=<Guard0xa8ca088>)
            p147 = call_r(ConstClass(rbigint.add), p144, ConstPtr(ptr146), descr=<Callr 8 rr EF=4>)
            guard_no_exception(descr=<Guard0xa8ca0f0>)
            i148 = getfield_gc_i(p147, descr=<FieldS rpython.rlib.rbigint.rbigint.inst_sign 16 pure>)
            i150 = int_ge(i148, 0)
            guard_true(i150, descr=<Guard0xa8a86e0>)
            i151 = getfield_gc_i(p147, descr=<FieldS rpython.rlib.rbigint.rbigint.inst_size 24 pure>)
            i153 = int_le(i151, 2)
            guard_true(i153, descr=<Guard0xa8a8728>)
            i155 = call_i(ConstClass(rbigint._touint_helper), p147, descr=<Calli 8 r EF=4>)
            guard_no_exception(descr=<Guard0xa8ca158>)
            i157 = int_ge(i155, 0)
            guard_true(i157, descr=<Guard0xa8a8770>)
            i159 = int_add(i120, 1)
            i161 = int_sub(i124, 1)
            setfield_gc(ConstPtr(ptr162), i161, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i164 = int_le(i161, 0)
            guard_false(i164, descr=<Guard0xa8ca1c0>)
            i165 = arraylen_gc(p55, descr=<ArrayP 8>)
            i166 = arraylen_gc(p76, descr=<ArrayP 8>)
            jump(p0, p1, i2, p3, p4, p7, p8, p10, i155, i159, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p55, p57, p59, p76, p78, i161, descr=TargetToken(176747472))
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
            i127 = arraylen_gc(p55, descr=<ArrayP 4>)
            i128 = arraylen_gc(p76, descr=<ArrayP 4>)
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
        guard_not_invalidated(descr=<Guard0x55adaa6b2b80>)
        i134 = int_le(i125, 1000000)
        guard_true(i134, descr=<Guard0x55adab89af50>)
        p136 = getfield_gc_r(ConstPtr(ptr135), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p136, ConstPtr(ptr137), descr=<Guard0x55adaa6b2be8>)
        p138 = force_token()
        enter_portal_frame(1, 0)
        p142 = getfield_gc_r(ConstPtr(ptr141), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p142, ConstPtr(ptr143), descr=<Guard0x55adaa6b2c50>)
        p145 = getfield_gc_r(ConstPtr(ptr144), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 16>)
        p147 = getarrayitem_gc_r(p145, 1, descr=<ArrayP 8>)
        guard_nonnull_class(p147, 94204358908784, descr=<Guard0x55adaa6b2cb8>)
        i149 = getfield_gc_i(p12, descr=<FieldS rsqueakvm.model.numeric.W_LargeIntegerBig.inst__exposed_size 32>)
        i151 = cond_call_value_i(i149, ConstClass(calculate_exposed_size_for_big_int), p72, descr=<Calli 8 r EF=4>)
        guard_no_exception(descr=<Guard0x55adaa6b2d20>)
        p153 = getfield_gc_r(ConstPtr(ptr152), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        setfield_gc(p12, i151, descr=<FieldS rsqueakvm.model.numeric.W_LargeIntegerBig.inst__exposed_size 32>)
        guard_value(p153, ConstPtr(ptr154), descr=<Guard0x55adaa6b2d88>)
        p155 = getfield_gc_r(p153, descr=<FieldP rsqueakvm.storage.AbstractCachingShadow.inst_version 32 pure>)
        guard_value(p155, ConstPtr(ptr156), descr=<Guard0x55adab89af98>)
        p157 = getfield_gc_r(p147, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p157, ConstPtr(ptr158), descr=<Guard0x55adaa6b2df0>)
        p159 = getfield_gc_r(p157, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        guard_value(p159, ConstPtr(ptr160), descr=<Guard0x55adab89afe0>)
        p162 = getfield_gc_r(ConstPtr(ptr161), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p162, ConstPtr(ptr163), descr=<Guard0x55adaa6b2e58>)
        p164 = force_token()
        enter_portal_frame(1, 0)
        i168 = int_lt(i151, 0)
        guard_false(i168, descr=<Guard0x55adab89b070>)
        p169 = new_array_clear(i151, descr=<ArrayU 1>)
        p171 = getfield_gc_r(ConstPtr(ptr170), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p171, ConstPtr(ptr172), descr=<Guard0x55adaa6b2ec0>)
        leave_portal_frame(1)
        p174 = force_token()
        enter_portal_frame(1, 0)
        p178 = getfield_gc_r(ConstPtr(ptr177), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p178, ConstPtr(ptr179), descr=<Guard0x55adaa6b2f28>)
        p180 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        i185 = int_sub(i151, 1)
        i186 = int_le(i151, i185)
        guard_false(i186, descr=<Guard0x55adab89b190>)
        p187 = force_token()
        p188 = new_with_vtable(descr=<SizeDescr 56>)
        p189 = new_with_vtable(descr=<SizeDescr 8>)
        setfield_gc(p188, ConstPtr(ptr190), descr=<FieldP rsqueakvm.model.base.W_AbstractObjectWithClassReference.inst_w_class 16 pure>)
        setfield_gc(p0, p187, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        setfield_gc(p188, 0, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8>)
        setfield_gc(p188, p169, descr=<FieldP rsqueakvm.model.variable.W_BytesObject.inst_bytes 32>)
        setfield_gc(p188, p189, descr=<FieldP rsqueakvm.model.variable.W_BytesObject.inst_version 40 pure>)
        call_may_force_n(ConstClass(_replace_from_to_trampoline__v124___simple_call__function__), 0, i185, 0, p188, p12, descr=<Callv 0 iiirr EF=7>)
        guard_not_forced(descr=<Guard0x55adac1fa560>)
        guard_no_exception(descr=<Guard0x55adab89b220>)
        guard_not_invalidated(descr=<Guard0x55adab89b2b0>)
        leave_portal_frame(1)
        leave_portal_frame(1)
        p197 = getfield_gc_r(p7, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        p198 = getfield_gc_r(p197, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        guard_value(p198, ConstPtr(ptr199), descr=<Guard0x55adab89b418>)
        i201 = int_add(i125, 1)
        i203 = getfield_gc_i(ConstPtr(ptr202), descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i205 = int_sub(i203, 1)
        setfield_gc(ConstPtr(ptr206), i205, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i208 = int_le(i205, 0)
        guard_false(i208, descr=<Guard0x55adaa6b2ff8>)
        jump(p0, p1, i2, p4, p6, p7, p9, p12, i201, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p197, p72, descr=TargetToken(94204432495648))
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
        guard_not_invalidated(descr=<Guard0x5599fb145880>)
        i178 = int_lt(i70, i55)
        guard_true(i178, descr=<Guard0x5599fd558338>)
        guard_value(p167, ConstPtr(ptr179), descr=<Guard0x5599fb1458e8>)
        p180 = getfield_gc_r(p7, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 16>)
        i182 = getarrayitem_gc_i(p180, 0, descr=<ArrayS 8>)
        i184 = int_eq(i182, 9223372036854775807)
        guard_false(i184, descr=<Guard0x5599fd5582f0>)
        i186 = getarrayitem_gc_i(p180, 2, descr=<ArrayS 8>)
        i188 = int_eq(i186, 9223372036854775807)
        guard_false(i188, descr=<Guard0x5599fd5582a8>)
        i189 = int_mul_ovf(i70, i186)
        guard_no_overflow(descr=<Guard0x5599fd558260>)
        i190 = int_add_ovf(i182, i189)
        guard_no_overflow(descr=<Guard0x5599fd558218>)
        i192 = int_add(i70, 1)
        p194 = getarrayitem_gc_r(p77, 0, descr=<ArrayP 8>)
        guard_nonnull_class(p194, ConstClass(W_LargeIntegerBig), descr=<Guard0x5599fb145818>)
        p197 = getarrayitem_gc_r(p77, 1, descr=<ArrayP 8>)
        guard_nonnull_class(p197, 94119807871856, descr=<Guard0x5599fb144b18>)
        p199 = force_token()
        enter_portal_frame(1, 0)
        p202 = getfield_gc_r(p194, descr=<FieldP rsqueakvm.model.base.W_AbstractObjectWithClassReference.inst_w_class 16 pure>)
        guard_value(p202, ConstPtr(ptr203), descr=<Guard0x5599fd558380>)
        p205 = getfield_gc_r(ConstPtr(ptr204), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p205, ConstPtr(ptr206), descr=<Guard0x5599fb145950>)
        p207 = force_token()
        enter_portal_frame(1, 0)
        p211 = getfield_gc_r(ConstPtr(ptr210), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p211, ConstPtr(ptr212), descr=<Guard0x5599fb1459b8>)
        p214 = getfield_gc_r(ConstPtr(ptr213), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 16>)
        p216 = getarrayitem_gc_r(p214, 1, descr=<ArrayP 8>)
        guard_nonnull_class(p216, 94119807871856, descr=<Guard0x5599fb512430>)
        i218 = getfield_gc_i(p194, descr=<FieldS rsqueakvm.model.numeric.W_LargeIntegerBig.inst__exposed_size 32>)
        p219 = getfield_gc_r(p194, descr=<FieldP rsqueakvm.model.numeric.W_LargeIntegerBig.inst_value 40 pure>)
        i221 = cond_call_value_i(i218, ConstClass(calculate_exposed_size_for_big_int), p219, descr=<Calli 8 r EF=4>)
        guard_no_exception(descr=<Guard0x5599fb512638>)
        p223 = getfield_gc_r(ConstPtr(ptr222), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        setfield_gc(p194, i221, descr=<FieldS rsqueakvm.model.numeric.W_LargeIntegerBig.inst__exposed_size 32>)
        guard_value(p223, ConstPtr(ptr224), descr=<Guard0x5599fb5127d8>)
        p225 = getfield_gc_r(p223, descr=<FieldP rsqueakvm.storage.AbstractCachingShadow.inst_version 32 pure>)
        guard_value(p225, ConstPtr(ptr226), descr=<Guard0x5599fd5583c8>)
        p227 = getfield_gc_r(p216, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p227, ConstPtr(ptr228), descr=<Guard0x5599fb512e58>)
        p229 = getfield_gc_r(p227, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        guard_value(p229, ConstPtr(ptr230), descr=<Guard0x5599fd558410>)
        p232 = getfield_gc_r(ConstPtr(ptr231), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p232, ConstPtr(ptr233), descr=<Guard0x5599fb512ff8>)
        p234 = force_token()
        enter_portal_frame(1, 0)
        i238 = int_lt(i221, 0)
        guard_false(i238, descr=<Guard0x5599fd558458>)
        p239 = new_array_clear(i221, descr=<ArrayU 1>)
        p241 = getfield_gc_r(ConstPtr(ptr240), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p241, ConstPtr(ptr242), descr=<Guard0x5599fb513060>)
        leave_portal_frame(1)
        p244 = force_token()
        enter_portal_frame(1, 0)
        p248 = getfield_gc_r(ConstPtr(ptr247), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p248, ConstPtr(ptr249), descr=<Guard0x5599fb5133a0>)
        p250 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        i255 = int_sub(i221, 1)
        i256 = int_le(i221, i255)
        guard_false(i256, descr=<Guard0x5599fd5584a0>)
        p257 = force_token()
        p258 = new_with_vtable(descr=<SizeDescr 56>)
        p259 = new_with_vtable(descr=<SizeDescr 8>)
        setfield_gc(p258, p259, descr=<FieldP rsqueakvm.model.variable.W_BytesObject.inst_version 40 pure>)
        setfield_gc(p258, ConstPtr(ptr260), descr=<FieldP rsqueakvm.model.base.W_AbstractObjectWithClassReference.inst_w_class 16 pure>)
        setfield_gc(p0, p257, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        setfield_gc(p258, 0, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8>)
        setfield_gc(p258, p239, descr=<FieldP rsqueakvm.model.variable.W_BytesObject.inst_bytes 32>)
        call_may_force_n(ConstClass(_replace_from_to_trampoline__v124___simple_call__function__), 0, i255, 0, p258, p194, descr=<Callv 0 iiirr EF=7>)
        guard_not_forced(descr=<Guard0x5599fd4ea4f0>)
        guard_no_exception(descr=<Guard0x5599fd5584e8>)
        guard_not_invalidated(descr=<Guard0x5599fd558530>)
        leave_portal_frame(1)
        leave_portal_frame(1)
        p267 = getfield_gc_r(p75, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        p268 = getfield_gc_r(p267, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        guard_value(p268, ConstPtr(ptr269), descr=<Guard0x5599fd558578>)
        p270 = getfield_gc_r(p197, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p270, ConstPtr(ptr271), descr=<Guard0x5599fb513818>)
        p273 = getfield_gc_r(ConstPtr(ptr272), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p273, ConstPtr(ptr274), descr=<Guard0x5599fb513a20>)
        p275 = getfield_gc_r(p197, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 16>)
        leave_portal_frame(1)
        p277 = getfield_gc_r(p7, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        p278 = getfield_gc_r(p277, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        setarrayitem_gc(p275, 0, p258, descr=<ArrayP 8>)
        guard_value(p278, ConstPtr(ptr280), descr=<Guard0x5599fb513b58>)
        i282 = getfield_gc_i(ConstPtr(ptr281), descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i284 = int_sub(i282, 1)
        setfield_gc(ConstPtr(ptr285), i284, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i287 = int_le(i284, 0)
        guard_false(i287, descr=<Guard0x5599fb87c770>)
        i288 = arraylen_gc(p77, descr=<ArrayP 8>)
        jump(p0, p1, i2, p4, p6, p7, p9, p12, i190, i192, p18, p258, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p277, i55, p77, p75, p267, descr=TargetToken(94119868618880))
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
        guard_not_invalidated(descr=<Guard0x55d2d85d95a8>)
        i73 = int_le(i64, 10000)
        guard_true(i73, descr=<Guard0x55d2da18c0f8>)
        i74 = getfield_gc_i(p12, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8>)
        i76 = cond_call_value_i(i74, ConstClass(calculate_and_cache), p12, descr=<Calli 8 r EF=5>)
        guard_no_exception(descr=<Guard0x55d2d85d9610>)
        i78 = int_add(i64, 1)
        i80 = int_sub(i68, 1)
        setfield_gc(ConstPtr(ptr81), i80, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i83 = int_le(i80, 0)
        guard_false(i83, descr=<Guard0x55d2d85d9678>)
        jump(p0, p1, i2, p4, p6, p7, p9, p12, i78, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p47, i80, descr=TargetToken(94364117135856))
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
        guard_not_invalidated(descr=<Guard0x55a7d3b0ed88>)
        i119 = int_le(i110, 10000)
        guard_true(i119, descr=<Guard0x55a7d4bccc80>)
        p120 = force_token()
        enter_portal_frame(1, 0)
        i123 = int_add_ovf(i110, i74)
        guard_no_overflow(descr=<Guard0x55a7d4bcccc8>)
        i125 = int_sub(i123, 1)
        i126 = int_gt(i125, i81)
        guard_false(i126, descr=<Guard0x55a7d4bccd10>)
        i128 = int_sub(i125, 1)
        i129 = uint_lt(i128, i94)
        guard_true(i129, descr=<Guard0x55a7d4bccd58>)
        i131 = int_lt(i128, 0)
        guard_false(i131, descr=<Guard0x55a7d4bccda0>)
        p132 = getarrayitem_gc_r(p93, i128, descr=<ArrayP 8>)
        guard_nonnull_class(p132, 94179238574960, descr=<Guard0x55a7d3b0edf0>)
        leave_portal_frame(1)
        p135 = getfield_gc_r(p132, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p135, ConstPtr(ptr136), descr=<Guard0x55a7d3b0ee58>)
        i137 = getfield_gc_i(p132, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8>)
        i139 = cond_call_value_i(i137, ConstClass(calculate_and_cache), p132, descr=<Calli 8 r EF=5>)
        guard_no_exception(descr=<Guard0x55a7d3b0eec0>)
        i141 = int_add(i110, 1)
        i143 = int_sub(i114, 1)
        setfield_gc(ConstPtr(ptr144), i143, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i146 = int_le(i143, 0)
        guard_false(i146, descr=<Guard0x55a7d3b0ef28>)
        i148 = arraylen_gc(p70, descr=<ArrayP 8>)
        jump(p0, p1, i2, p4, p6, p7, p9, p12, p14, p16, p18, i141, p26, p28, p30, p32, p34, p36, p38, p40, p42, p47, p70, p72, i74, p79, i81, p84, i94, p93, i143, descr=TargetToken(94179317062640))
        """)

    def test_nested_closure_loop_call(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        1 to: 10000 do: [:a |
            (1 to: 1000) do: [:b |
                b abs.
            ]
        ].
        """)
        # important: there should be two loops, with the outer calling the inner, and in between an entry bridge to the inner loop abs call
        self.assert_matches(traces[-3].loop, """
        guard_not_invalidated(descr=<Guard0x4f86cb8>)
        i106 = int_lt(i66, i49)
        guard_true(i106, descr=<Guard0x6a78f98>)
        i107 = int_mul_ovf(i66, i60)
        guard_no_overflow(descr=<Guard0x6a78f50>)
        i108 = int_add_ovf(i56, i107)
        guard_no_overflow(descr=<Guard0x6a78ec0>)
        i110 = int_add(i66, 1)
        p111 = force_token()
        enter_portal_frame(1, 0)
        p114 = force_token()
        enter_portal_frame(1, 0)
        i118 = int_lt(i108, 0)
        guard_false(i118, descr=<Guard0x6a79070>)
        leave_portal_frame(1)
        leave_portal_frame(1)
        i122 = int_sub(i102, 1)
        setfield_gc(ConstPtr(ptr123), i122, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i125 = int_le(i122, 0)
        guard_false(i125, descr=<Guard0x4f87198>)
        i127 = arraylen_gc(p54, descr=<ArrayS 8>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i108, i110, p17, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i49, p54, i60, i56, i122, descr=TargetToken(111727328))
        """)
        assert len(traces[-2].setup) > 0
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0x5557b0fb0770>)
        i198 = int_le(i189, 10000)
        guard_true(i198, descr=<Guard0x5557b2f606e0>)
        p200 = getfield_gc_r(ConstPtr(ptr199), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p200, ConstPtr(ptr201), descr=<Guard0x5557b1bcc910>)
        p202 = force_token()
        enter_portal_frame(1, 0)
        p206 = getfield_gc_r(ConstPtr(ptr205), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p206, ConstPtr(ptr207), descr=<Guard0x5557b1bcc8a8>)
        p209 = getfield_gc_r(ConstPtr(ptr208), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 16>)
        p211 = getarrayitem_gc_r(p209, 1, descr=<ArrayP 8>)
        guard_nonnull_class(p211, 93835094563696, descr=<Guard0x5557b1bcc840>)
        p213 = getfield_gc_r(p211, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p213, ConstPtr(ptr214), descr=<Guard0x5557b1f06020>)
        p215 = getfield_gc_r(p213, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        guard_value(p215, ConstPtr(ptr216), descr=<Guard0x5557b2f60698>)
        p218 = getfield_gc_r(ConstPtr(ptr217), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p218, ConstPtr(ptr219), descr=<Guard0x5557b1f86f90>)
        p220 = force_token()
        enter_portal_frame(1, 0)
        p224 = getfield_gc_r(ConstPtr(ptr223), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p224, ConstPtr(ptr225), descr=<Guard0x5557b0f639b8>)
        p226 = getfield_gc_r(p224, descr=<FieldP rsqueakvm.storage.AbstractCachingShadow.inst_version 32 pure>)
        guard_value(p226, ConstPtr(ptr227), descr=<Guard0x5557b2f60650>)
        p229 = getfield_gc_r(ConstPtr(ptr228), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_value(p229, ConstPtr(ptr230), descr=<Guard0x5557b0f63950>)
        p231 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        leave_portal_frame(1)
        p237 = getfield_gc_r(p1, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        guard_nonnull_class(p237, ConstClass(ContextPartShadow), descr=<Guard0x5557b0f638e8>)
        i239 = ptr_eq(p237, p0)
        guard_true(i239, descr=<Guard0x5557b2f605c0>)
        p240 = force_token()
        enter_portal_frame(1, 0)
        p243 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        p247 = force_token()
        enter_portal_frame(1, 0)
        p250 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        i256 = int_sub(i193, 1)
        setfield_gc(ConstPtr(ptr257), i256, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i259 = int_le(i256, 0)
        guard_false(i259, descr=<Guard0x5557b0fb1cf8>)
        p260 = force_token()
        p261 = new_with_vtable(descr=<SizeDescr 104>)
        p262 = new_with_vtable(descr=<SizeDescr 24>)
        setfield_gc(p262, p240, descr=<FieldP JitVirtualRef.virtual_token 8>)
        setfield_gc(p262, ConstPtr(null), descr=<FieldP JitVirtualRef.forced 16>)
        p264 = new_array_clear(17, descr=<ArrayP 8>)
        p265 = new_with_vtable(descr=<SizeDescr 72>)
        setfield_gc(p265, 0, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8>)
        setfield_gc(p265, ConstPtr(ptr267), descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__stack 16 pure>)
        setfield_gc(p265, 268435503, descr=<FieldU rsqueakvm.model.block_closure.W_BlockClosure.inst__startpc_stacklen_args 24 pure>)
        setfield_gc(p265, ConstPtr(ptr269), descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__w_method 32 pure>)
        setfield_gc(p265, p1, descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__w_outerContext 40 pure>)
        setfield_gc(p265, p7, descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst__w_receiver 48 pure>)
        setfield_gc(p265, ConstPtr(ptr270), descr=<FieldP rsqueakvm.model.block_closure.W_BlockClosure.inst_version 56 pure>)
        setarrayitem_gc(p264, 0, p265, descr=<ArrayP 8>)
        p272 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p272, 1, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p264, 1, p272, descr=<ArrayP 8>)
        p275 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p275, 1, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p264, 2, p275, descr=<ArrayP 8>)
        p278 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p278, 1000, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p264, 3, p278, descr=<ArrayP 8>)
        p281 = new_with_vtable(descr=<SizeDescr 16>)
        setfield_gc(p281, 1, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8 pure>)
        setarrayitem_gc(p264, 4, p281, descr=<ArrayP 8>)
        setarrayitem_gc(p264, 5, ConstPtr(ptr285), descr=<ArrayP 8>)
        setarrayitem_gc(p264, 6, ConstPtr(ptr287), descr=<ArrayP 8>)
        p288 = new_with_vtable(descr=<SizeDescr 32>)
        setfield_gc(p288, 0, descr=<FieldS rsqueakvm.model.base.W_AbstractObjectWithIdentityHash.inst_hash 8>)
        p291 = new_array(3, descr=<ArrayS 8>)
        setarrayitem_gc(p291, 0, 1, descr=<ArrayS 8>)
        setarrayitem_gc(p291, 1, 1000, descr=<ArrayS 8>)
        setarrayitem_gc(p291, 2, 1, descr=<ArrayS 8>)
        setfield_gc(p288, p291, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 16>)
        setfield_gc(p288, ConstPtr(ptr298), descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        setfield_gc(p261, ConstPtr(ptr299), descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        setfield_gc(p0, p260, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        setfield_gc(p261, p262, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>)
        setfield_gc(p261, 1090519045, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        setfield_gc(p261, p264, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__temps_and_stack 40>)
        setfield_gc(p261, ConstPtr(ptr301), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 48>)
        setfield_gc(p261, p288, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_receiver 56>)
        setfield_gc(p261, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self 64>)
        setfield_gc(p261, 23, descr=<FieldS rsqueakvm.storage_contexts.ContextPartShadow.inst__w_self_size 72>)
        setfield_gc(p261, ConstPtr(ptr304), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_blockmethod 80>)
        setfield_gc(p261, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 88>)
        setfield_gc(p261, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_extra_data 96>)
        call_assembler_n(p261, descr=<Loop1>)
        guard_not_forced(descr=<Guard0x5557b2c1c800>)
        keepalive(p261)
        p308 = guard_exception(93835094565040, descr=<Guard0x5557b0fb17b0>)
        i309 = ptr_eq(p261, p0)
        guard_false(i309, descr=<Guard0x5557b0fb12d0>)
        p310 = getfield_gc_r(p261, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.vable_token 16>)
        i312 = ptr_ne(p310, ConstPtr(null))
        cond_call(i312, 93835087040928, p261, descr=<Callv 0 r EF=2 OS=121>)
        i314 = getfield_gc_i(p261, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        guard_value(i314, 1090519067, descr=<Guard0x5557b0fb1268>)
        guard_not_invalidated(descr=<Guard0x5557b2f60578>)
        p316 = getfield_gc_r(p261, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst_closure 88>)
        guard_isnull(p316, descr=<Guard0x5557b2f60530>)
        p317 = getfield_gc_r(p261, descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__w_method 48>)
        guard_value(p317, ConstPtr(ptr318), descr=<Guard0x5557b0fb1200>)
        setfield_gc(p261, ConstPtr(null), descr=<FieldP rsqueakvm.storage_contexts.ContextPartShadow.inst__s_sender 24>)
        setfield_gc(p261, 1094713343, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        guard_class(p308, 93835094565040, descr=<Guard0x5557b0fb1198>)
        p322 = getfield_gc_r(p308, descr=<FieldP rsqueakvm.interpreter.WrappedLocalReturn.inst_w_value 8 pure>)
        setfield_gc(p261, 20971519, descr=<FieldU rsqueakvm.storage_contexts.ContextPartShadow.inst__state_stackptr_pc 32>)
        setfield_gc(p262, ConstPtr(null), descr=<FieldP JitVirtualRef.virtual_token 8>)
        guard_nonnull(p322, descr=<Guard0x5557b0fb1130>)
        p324 = getfield_gc_r(p7, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 24>)
        p325 = getfield_gc_r(p324, descr=<FieldP rsqueakvm.storage.AbstractStrategy.inst_w_class 8 pure>)
        guard_value(p325, ConstPtr(ptr326), descr=<Guard0x5557b2f604e8>)
        i328 = int_add(i189, 1)
        i330 = getfield_gc_i(ConstPtr(ptr329), descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i332 = int_sub(i330, 1)
        setfield_gc(ConstPtr(ptr333), i332, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i335 = int_le(i332, 0)
        guard_false(i335, descr=<Guard0x5557b0fb07d8>)
        jump(p0, p1, i2, p4, p6, p7, p9, i328, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p324, i332, descr=TargetToken(93835149719440))
        """)

    def test_loop_unrolling(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | c instance |
        Object
            subclass: #MyA
            instanceVariableNames: ''
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := Smalltalk at: #MyA.
        c compile: 'oneIterLoop
1 to: 1 do: [:i | ].' classified: 'none' withStamp: nil notifying: nil logSource: false.
        instance := c new.
        1 to: 100000 do: [:i | instance oneIterLoop ].
        """)
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0x4a522f8>)
        i74 = int_le(i65, 100000)
        guard_true(i74, descr=<Guard0x4f28a88>)
        p75 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        i80 = int_add(i65, 1)
        i82 = int_sub(i69, 1)
        setfield_gc(ConstPtr(ptr83), i82, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i85 = int_le(i82, 0)
        guard_false(i85, descr=<Guard0x4a52360>)
        jump(p0, p1, i2, p4, p6, p7, p9, p12, p14, i80, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p47, i82, descr=TargetToken(83088416))
        """)

    def test_loop_unrolling2(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | c instance |
        Object
            subclass: #MyA
            instanceVariableNames: ''
            classVariableNames: ''
            poolDictionaries: ''
            category: 'Test'.
        c := Smalltalk at: #MyA.
        c compile: 'twoIterLoop
1 to: 2 do: [:i | ].' classified: 'none' withStamp: nil notifying: nil logSource: false.
        instance := c new.
        1 to: 100000 do: [:i | instance twoIterLoop ].
        """)
        self.assert_matches(traces[-1].loop, """
        guard_not_invalidated(descr=<Guard0x4a522f8>)
        i74 = int_le(i65, 100000)
        guard_true(i74, descr=<Guard0x4f28a88>)
        p75 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        i80 = int_add(i65, 1)
        i82 = int_sub(i69, 1)
        setfield_gc(ConstPtr(ptr83), i82, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i85 = int_le(i82, 0)
        guard_false(i85, descr=<Guard0x4a52360>)
        jump(p0, p1, i2, p4, p6, p7, p9, p12, p14, i80, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p47, i82, descr=TargetToken(83088416))
        """)
