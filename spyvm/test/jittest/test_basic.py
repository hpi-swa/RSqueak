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
        f18 = call(ConstClass(ll_time.ll_time_time), descr=<Callf 8 EF=4>)
        setfield_gc(ConstPtr(ptr19), 10000, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        guard_no_exception(descr=<Guard0x9732d30>)
        f22 = float_sub(f18, 1387380038.806162)
        f24 = float_mul(f22, 1000.000000)
        i25 = cast_float_to_int(f24)
        i27 = int_and(i25, 2147483647)
        i28 = getfield_gc(ConstPtr(ptr19), descr=<FieldS spyvm.interpreter.Interpreter.inst_next_wakeup_tick 36>)
        i29 = int_is_zero(i28)
        guard_true(i29, descr=<Guard0x9761ad8>)
        label(p0, p1, i16, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, descr=TargetToken(158475216))
        guard_class(p0, ConstClass(MethodContextShadow), descr=<Guard0x9761a9c>)
        p31 = getfield_gc(p0, descr=<FieldP spyvm.shadow.MethodContextShadow.inst__w_method 44>)
        guard_value(p31, ConstPtr(ptr32), descr=<Guard0x9761a60>)
        guard_not_invalidated(descr=<Guard0x9761a24>)
        i34 = int_le(i16, 1000000000)
        guard_true(i34, descr=<Guard0x97619e8>)
        i36 = int_add(i16, 1)
        i38 = int_sub(i36, -1073741824)
        i40 = uint_lt(i38, -2147483648)
        guard_true(i40, descr=<Guard0x97619ac>)
        setfield_gc(ConstPtr(ptr19), 9999, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        jump(p0, p1, i36, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, 9999, descr=TargetToken(158474976))
        """)

    def test_constant_string(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | i |
        i := 0.
        [i <= 10000] whileTrue: [ i := i + 'a' size].
        ^ i
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x92520c4>)
        i68 = int_le(i58, 10000)
        guard_true(i68, descr=<Guard0x9252088>)
        i69 = int_add(i58, 1)
        i70 = int_sub(i69, -1073741824)
        i71 = uint_lt(i70, -2147483648)
        guard_true(i71, descr=<Guard0x925204c>)
        i72 = int_sub(i65, 1)
        setfield_gc(ConstPtr(ptr55), i72, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 16>)
        i73 = int_le(i72, 0)
        guard_false(i73, descr=<Guard0x9252010>)
        jump(p0, p3, i69, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i72, descr=TargetToken(153187472))
        """)

    def test_constant_string_equal2(self, spy, tmpdir):
        # This used to have a call to array comparison in it
        traces = self.run(spy, tmpdir, """
        | i |
        i := 0.
        [i <= 100000] whileTrue: [
          'a' == 'ab'.
          'cde' == 'efg'.
          'hij' == 'hij'.
          i := i + 1].
        ^ i
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x9c66a60>),
        i76 = int_le(i65, 100000),
        guard_true(i76, descr=<Guard0x9c66628>),
        i77 = int_add(i65, 1),
        i78 = int_sub(i77, -1073741824),
        i79 = uint_lt(i78, -2147483648),
        guard_true(i79, descr=<Guard0x9c622a4>),
        i80 = int_sub(i73, 2),
        setfield_gc(ConstPtr(ptr70), i80, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
        i81 = int_le(i80, 0),
        guard_false(i81, descr=<Guard0x9bbbe5c>),
        i83 = arraylen_gc(p49, descr=<ArrayU 1>),
        i84 = arraylen_gc(p53, descr=<ArrayU 1>),
        i85 = arraylen_gc(p57, descr=<ArrayU 1>),
        jump(p0, p3, i77, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i80, p49, p53, p57, descr=TargetToken(163738864))
        """)

    def test_constant_string_var_equal(self, spy, tmpdir):
        # This used to have a call to array comparison in it
        traces = self.run(spy, tmpdir, """
        | i a b c d |
        i := 0.
        a = 'a'.
        b = 'bc'.
        c = 'cd'.
        d = 'bc'.
        [i <= 100000] whileTrue: [
          a == b.
          b == c.
          b == d.
          i := i + 1].
        ^ i
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x967e7cc>),
        i73 = int_le(i62, 100000),
        guard_true(i73, descr=<Guard0x967e790>),
        i74 = int_add(i62, 1),
        i75 = int_sub(i74, -1073741824),
        i76 = uint_lt(i75, -2147483648),
        guard_true(i76, descr=<Guard0x967e754>),
        i77 = int_sub(i70, 1),
        setfield_gc(ConstPtr(ptr67), i77, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
        i78 = int_le(i77, 0),
        guard_false(i78, descr=<Guard0x967e718>),
        jump(p0, p3, i74, p8, p10, p12, p14, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i77, descr=TargetToken(157713840))
        """)
