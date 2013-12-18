import py

from .base import BaseJITTest


class TestBasic(BaseJITTest):
    def test_while_loop(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        0 to: 1000000000 do: [:t|nil].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xa15ec7c>)
        i60 = int_le(i49, 10000)
        guard_true(i60, descr=<Guard0xa15ec40>)
        i61 = int_add(i49, 1)
        i62 = int_sub(i61, -1073741824)
        i63 = uint_lt(i62, -2147483648)
        guard_true(i63, descr=<Guard0xa15ec04>)
        i64 = int_sub(i57, 1)
        setfield_gc(ConstPtr(ptr54), i64, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 16>)
        i65 = int_le(i64, 0)
        guard_false(i65, descr=<Guard0xa15ebc8>)
        jump(p0, p3, i61, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i64, descr=TargetToken(169145008))
        """)
        self.assert_matches(traces[0].bridges[0], """
        i18 = getfield_gc(ConstPtr(ptr17), descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_counter_size 20>),
        f20 = call(ConstClass(ll_time.ll_time_time), descr=<Callf 8 EF=4>),
        setfield_gc(ConstPtr(ptr17), i18, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 16>),
        guard_no_exception(descr=<Guard0x9d3ee5c>),
        f22 = float_mul(f20, 1000.000000),
        call(ConstClass(set_errno), 0, descr=<Callv 0 i EF=2>),
        f27 = call(ConstClass(fmod), f22, 536870911.000000, descr=<Callf 8 ff EF=2>),
        i29 = call(ConstClass(get_errno), descr=<Calli 4 EF=2>),
        i30 = float_ne(f27, f27),
        guard_false(i30, descr=<Guard0x9d6ce20>),
        i31 = int_is_true(i29),
        guard_false(i31, descr=<Guard0x9d6cde4>),
        i32 = cast_float_to_int(f27),
        i33 = getfield_gc(ConstPtr(ptr17), descr=<FieldS spyvm.interpreter.Interpreter.inst_next_wakeup_tick 28>),
        i34 = int_is_zero(i33),
        guard_true(i34, descr=<Guard0x9d6cda8>),
        i35 = same_as(i18),
        label(p0, p1, i16, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, i35, descr=TargetToken(164815776)),
        guard_class(p0, ConstClass(MethodContextShadow), descr=<Guard0x9d6cd6c>),
        p37 = getfield_gc(p0, descr=<FieldP spyvm.shadow.MethodContextShadow.inst__w_method 44>),
        guard_value(p37, ConstPtr(ptr38), descr=<Guard0x9d6cd30>),
        guard_not_invalidated(descr=<Guard0x9d6ccf4>),
        i40 = int_le(i16, 1000000000),
        guard_true(i40, descr=<Guard0x9d6ccb8>),
        i42 = int_add(i16, 1),
        i44 = int_sub(i42, -1073741824),
        i46 = uint_lt(i44, -2147483648),
        guard_true(i46, descr=<Guard0x9d6cc7c>),
        i48 = int_sub(i35, 1),
        setfield_gc(ConstPtr(ptr17), i48, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 16>),
        i50 = int_le(i48, 0),
        guard_false(i50, descr=<Guard0x9d6cc40>),
        jump(p0, p1, i42, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, i48, descr=TargetToken(164815536))
        """)

    def test_constant_string(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | i |
        i := 0.
        [i <= 10000] whileTrue: [ i := i + 'a' size].
        ^ i
        """)
        self.assert_matches(traces[0].loop, """
        label(p0, p3, i58, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i65, descr=TargetToken(153187472))
        debug_merge_point(0, 0, '2: [0x10]pushTemporaryVariableBytecode (codeTest1387373494)')
        guard_not_invalidated(descr=<Guard0x92520c4>)
        debug_merge_point(0, 0, '3: [0x21]pushLiteralConstantBytecode (codeTest1387373494)')
        debug_merge_point(0, 0, '4: [0xb4]bytecodePrimLessOrEqual (codeTest1387373494)')
        i68 = int_le(i58, 10000)
        guard_true(i68, descr=<Guard0x9252088>)
        debug_merge_point(0, 0, '5: [0x9e]shortConditionalJump (codeTest1387373494)')
        debug_merge_point(0, 0, '6: [0x10]pushTemporaryVariableBytecode (codeTest1387373494)')
        debug_merge_point(0, 0, '7: [0x20]pushLiteralConstantBytecode (codeTest1387373494)')
        debug_merge_point(0, 0, '8: [0xc2]bytecodePrimSize (codeTest1387373494)')
        debug_merge_point(0, 0, '9: [0xb0]bytecodePrimAdd (codeTest1387373494)')
        i69 = int_add(i58, 1)
        i70 = int_sub(i69, -1073741824)
        i71 = uint_lt(i70, -2147483648)
        guard_true(i71, descr=<Guard0x925204c>)
        debug_merge_point(0, 0, '10: [0x68]storeAndPopTemporaryVariableBytecode (codeTest1387373494)')
        debug_merge_point(0, 0, '11: [0xa3]longUnconditionalJump (codeTest1387373494)')
        i72 = int_sub(i65, 1)
        setfield_gc(ConstPtr(ptr55), i72, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 16>)
        i73 = int_le(i72, 0)
        guard_false(i73, descr=<Guard0x9252010>)
        debug_merge_point(0, 0, '2: [0x10]pushTemporaryVariableBytecode (codeTest1387373494)')
        jump(p0, p3, i69, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i72, descr=TargetToken(153187472))
        """)
