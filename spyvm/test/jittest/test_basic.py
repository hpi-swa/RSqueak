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
        i77 = int_sub(i70, 1),
        setfield_gc(ConstPtr(ptr67), i77, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
        i78 = int_le(i77, 0),
        guard_false(i78, descr=<Guard0x967e718>),
        jump(p0, p3, i74, p8, p10, p12, p14, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i77, descr=TargetToken(157713840))
        """)

    def test_bitblt_fillWhite(self, spy, tmpdir):
        # This used to have a call to array comparison in it
        traces = []
        retries = 10
        while len(traces) == 0 and retries > 0:
            retries -= 1
            traces = self.run(spy, tmpdir, """
            Display beDisplay.
            1 to: 10000 do: [:i | Display fillWhite].
            """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xa3523d0>),
        i540 = int_le(2, i151),
        guard_false(i540, descr=<Guard0xa34fde4>),
        i541 = getfield_gc_pure(p529, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
        i542 = int_add_ovf(i541, i158),
        guard_no_overflow(descr=<Guard0xa34f574>),
        i543 = getfield_gc_pure(p532, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
        i544 = int_add_ovf(i543, i165),
        guard_no_overflow(descr=<Guard0xa34dcf4>),
        i545 = int_add_ovf(i170, 1),
        guard_no_overflow(descr=<Guard0xa34d808>),
        i546 = int_sub(i525, 3),
        setfield_gc(ConstPtr(ptr171), i546, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
        i547 = int_le(i546, 0),
        guard_false(i547, descr=<Guard0xa34d5b0>),
        i548 = int_le(i545, i179),
        guard_true(i548, descr=<Guard0xa34aed4>),
        i549 = getfield_gc_pure(p535, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
        i550 = int_mod(i549, i197),
        i551 = int_rshift(i550, 31),
        i552 = int_and(i197, i551),
        i553 = int_add(i550, i552),
        i554 = int_add_ovf(1, i553),
        guard_no_overflow(descr=<Guard0xa347a9c>),
        i555 = int_ge(i553, 0),
        guard_true(i555, descr=<Guard0xa347664>),
        i556 = int_lt(i553, i197),
        guard_true(i556, descr=<Guard0xa347628>),
        i557 = getarrayitem_gc(p213, i553, descr=<ArrayU 4>),
        i558 = uint_lt(i557, 0),
        guard_false(i558, descr=<Guard0xa3475b0>),
        i559 = uint_lt(i557, 2147483647),
        guard_true(i559, descr=<Guard0xa347574>),
        i560 = int_add_ovf(i549, i221),
        guard_no_overflow(descr=<Guard0xa345e5c>),
        i561 = int_ge(i557, 0),
        guard_true(i561, descr=<Guard0xa340f4c>),
        i562 = int_and(i557, i557),
        i563 = uint_lt(i562, 2147483647),
        guard_true(i563, descr=<Guard0xa340ed4>),
        i564 = int_add_ovf(i544, 1),
        guard_no_overflow(descr=<Guard0xa340970>),
        i565 = int_ge(i544, 0),
        guard_true(i565, descr=<Guard0xa340538>),
        i566 = int_lt(i544, i250),
        guard_true(i566, descr=<Guard0xa3404fc>),
        i567 = getarrayitem_raw(i252, i544, descr=<ArrayU 4>),
        i568 = uint_lt(i567, 0),
        guard_false(i568, descr=<Guard0xa3404c0>),
        i569 = uint_lt(i567, 2147483647),
        guard_true(i569, descr=<Guard0xa340484>),
        i570 = int_ge(i562, 0),
        guard_true(i570, descr=<Guard0xa3382a4>),
        i571 = int_and(i282, i562),
        i572 = uint_lt(i571, 2147483647),
        guard_true(i572, descr=<Guard0xa338268>),
        i573 = getarrayitem_raw(i252, i544, descr=<ArrayU 4>),
        i574 = uint_lt(i573, 0),
        guard_false(i574, descr=<Guard0xa3336dc>),
        i575 = uint_lt(i573, 2147483647),
        guard_true(i575, descr=<Guard0xa3336a0>),
        i576 = int_ge(i573, 0),
        guard_true(i576, descr=<Guard0xa333574>),
        i577 = int_and(i293, i573),
        i578 = uint_lt(i577, 2147483647),
        guard_true(i578, descr=<Guard0xa333538>),
        i579 = int_ge(i571, 0),
        guard_true(i579, descr=<Guard0xa333448>),
        i580 = int_ge(i577, 0),
        guard_true(i580, descr=<Guard0xa33340c>),
        i581 = int_or(i571, i577),
        i582 = uint_lt(i581, 2147483647),
        guard_true(i582, descr=<Guard0xa3333d0>),
        setarrayitem_raw(i252, i544, i581, descr=<ArrayU 4>),
        i584 = int_lshift(i544, 3),
        i585 = int_ge(i584, i250),
        guard_false(i585, descr=<Guard0xa330e5c>),
        i586 = uint_rshift(i581, i328),
        i587 = int_lshift(i581, i315),
        i588 = uint_rshift(i587, i328),
        i589 = int_lshift(i588, 8),
        i590 = int_or(i586, i589),
        i591 = int_lshift(i587, i315),
        i592 = uint_rshift(i591, i328),
        i593 = int_lshift(i592, 16),
        i594 = int_or(i590, i593),
        i595 = int_lshift(i591, i315),
        i596 = uint_rshift(i595, i328),
        i597 = int_lshift(i596, 24),
        i598 = int_or(i594, i597),
        i599 = int_lshift(i595, i315),
        setarrayitem_raw(i349, i584, i598, descr=<ArrayU 4>),
        i600 = int_add(i584, 1),
        i601 = int_ge(i600, i250),
        guard_false(i601, descr=<Guard0xa330de4>),
        i602 = uint_rshift(i599, i328),
        i603 = int_lshift(i599, i315),
        i604 = uint_rshift(i603, i328),
        i605 = int_lshift(i604, 8),
        i606 = int_or(i602, i605),
        i607 = int_lshift(i603, i315),
        i608 = uint_rshift(i607, i328),
        i609 = int_lshift(i608, 16),
        i610 = int_or(i606, i609),
        i611 = int_lshift(i607, i315),
        i612 = uint_rshift(i611, i328),
        i613 = int_lshift(i612, 24),
        i614 = int_or(i610, i613),
        i615 = int_lshift(i611, i315),
        setarrayitem_raw(i349, i600, i614, descr=<ArrayU 4>),
        i616 = int_add(i600, 1),
        i617 = int_ge(i616, i250),
        guard_false(i617, descr=<Guard0xa330d6c>),
        i618 = uint_rshift(i615, i328),
        i619 = int_lshift(i615, i315),
        i620 = uint_rshift(i619, i328),
        i621 = int_lshift(i620, 8),
        i622 = int_or(i618, i621),
        i623 = int_lshift(i619, i315),
        i624 = uint_rshift(i623, i328),
        i625 = int_lshift(i624, 16),
        i626 = int_or(i622, i625),
        i627 = int_lshift(i623, i315),
        i628 = uint_rshift(i627, i328),
        i629 = int_lshift(i628, 24),
        i630 = int_or(i626, i629),
        i631 = int_lshift(i627, i315),
        setarrayitem_raw(i349, i616, i630, descr=<ArrayU 4>),
        i632 = int_add(i616, 1),
        i633 = int_ge(i632, i250),
        guard_false(i633, descr=<Guard0xa330cf4>),
        i634 = uint_rshift(i631, i328),
        i635 = int_lshift(i631, i315),
        i636 = uint_rshift(i635, i328),
        i637 = int_lshift(i636, 8),
        i638 = int_or(i634, i637),
        i639 = int_lshift(i635, i315),
        i640 = uint_rshift(i639, i328),
        i641 = int_lshift(i640, 16),
        i642 = int_or(i638, i641),
        i643 = int_lshift(i639, i315),
        i644 = uint_rshift(i643, i328),
        i645 = int_lshift(i644, 24),
        i646 = int_or(i642, i645),
        i647 = int_lshift(i643, i315),
        setarrayitem_raw(i349, i632, i646, descr=<ArrayU 4>),
        i648 = int_add(i632, 1),
        i649 = int_ge(i648, i250),
        guard_false(i649, descr=<Guard0xa330c7c>),
        i650 = uint_rshift(i647, i328),
        i651 = int_lshift(i647, i315),
        i652 = uint_rshift(i651, i328),
        i653 = int_lshift(i652, 8),
        i654 = int_or(i650, i653),
        i655 = int_lshift(i651, i315),
        i656 = uint_rshift(i655, i328),
        i657 = int_lshift(i656, 16),
        i658 = int_or(i654, i657),
        i659 = int_lshift(i655, i315),
        i660 = uint_rshift(i659, i328),
        i661 = int_lshift(i660, 24),
        i662 = int_or(i658, i661),
        i663 = int_lshift(i659, i315),
        setarrayitem_raw(i349, i648, i662, descr=<ArrayU 4>),
        i664 = int_add(i648, 1),
        i665 = int_ge(i664, i250),
        guard_false(i665, descr=<Guard0xa330c04>),
        i666 = uint_rshift(i663, i328),
        i667 = int_lshift(i663, i315),
        i668 = uint_rshift(i667, i328),
        i669 = int_lshift(i668, 8),
        i670 = int_or(i666, i669),
        i671 = int_lshift(i667, i315),
        i672 = uint_rshift(i671, i328),
        i673 = int_lshift(i672, 16),
        i674 = int_or(i670, i673),
        i675 = int_lshift(i671, i315),
        i676 = uint_rshift(i675, i328),
        i677 = int_lshift(i676, 24),
        i678 = int_or(i674, i677),
        i679 = int_lshift(i675, i315),
        setarrayitem_raw(i349, i664, i678, descr=<ArrayU 4>),
        i680 = int_add(i664, 1),
        i681 = int_ge(i680, i250),
        guard_false(i681, descr=<Guard0xa330b8c>),
        i682 = uint_rshift(i679, i328),
        i683 = int_lshift(i679, i315),
        i684 = uint_rshift(i683, i328),
        i685 = int_lshift(i684, 8),
        i686 = int_or(i682, i685),
        i687 = int_lshift(i683, i315),
        i688 = uint_rshift(i687, i328),
        i689 = int_lshift(i688, 16),
        i690 = int_or(i686, i689),
        i691 = int_lshift(i687, i315),
        i692 = uint_rshift(i691, i328),
        i693 = int_lshift(i692, 24),
        i694 = int_or(i690, i693),
        i695 = int_lshift(i691, i315),
        setarrayitem_raw(i349, i680, i694, descr=<ArrayU 4>),
        i696 = int_add(i680, 1),
        i697 = int_ge(i696, i250),
        guard_false(i697, descr=<Guard0xa330b14>),
        i698 = uint_rshift(i695, i328),
        i699 = int_lshift(i695, i315),
        i700 = uint_rshift(i699, i328),
        i701 = int_lshift(i700, 8),
        i702 = int_or(i698, i701),
        i703 = int_lshift(i699, i315),
        i704 = uint_rshift(i703, i328),
        i705 = int_lshift(i704, 16),
        i706 = int_or(i702, i705),
        i707 = int_lshift(i703, i315),
        i708 = uint_rshift(i707, i328),
        i709 = int_lshift(i708, 24),
        i710 = int_or(i706, i709),
        i711 = int_lshift(i707, i315),
        setarrayitem_raw(i349, i696, i710, descr=<ArrayU 4>),
        i712 = int_add(i696, 1),
        i713 = int_add_ovf(i542, i510),
        guard_no_overflow(descr=<Guard0xa33031c>),
        i714 = int_add_ovf(i544, i510),
        guard_no_overflow(descr=<Guard0xa32ea9c>),
        i715 = int_sub(i546, 26),
        setfield_gc(ConstPtr(ptr171), i715, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
        i716 = int_le(i715, 0),
        guard_false(i716, descr=<Guard0xa31e808>),
        p717 = new_with_vtable(ConstClass(W_SmallInteger)),
        setfield_gc(p717, i713, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
        setarrayitem_gc(p146, 34, p717, descr=<ArrayP 4>),
        p718 = new_with_vtable(ConstClass(W_SmallInteger)),
        setfield_gc(p718, i714, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
        setarrayitem_gc(p146, 35, p718, descr=<ArrayP 4>),
        p719 = new_with_vtable(ConstClass(W_SmallInteger)),
        setfield_gc(p719, i560, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
        setarrayitem_gc(p146, 20, p719, descr=<ArrayP 4>),
        i720 = arraylen_gc(p146, descr=<ArrayP 4>),
        i721 = arraylen_gc(p521, descr=<ArrayP 4>),
        jump(p0, p3, p8, i557, p538, i562, p18, i545, p38, p40, p42, p44, p46, p48, p50, p52, p54, p56, p58, p60, p62, p64, p66, p68, p70, p72, p74, p76, p78, p80, p82, p84, p86, p88, p90, p92, p94, p96, p98, p100, p102, p104, p106, p108, p110, p112, p114, p116, p118, p120, p122, p124, p126, p128, p130, p132, p134, 1, p148, p717, i158, p156, p718, i165, p163, p146, i715, i179, p178, p719, i197, p188, p213, i221, p220, p228, p140, p242, i250, i252, i282, i293, i328, i315, i349, i510, p509, p538, p521, descr=TargetToken(169555520))]
        """)
        
    @py.test.mark.skipif("'just dozens of long traces'")
    def test_bitblt_draw_windows(self, spy, tmpdir):
        # This used to have a call to array comparison in it
        traces = self.run(spy, tmpdir, """
        Display beDisplay.
        1 to: 100 do: [:i | ControlManager startUp].
        """)
        self.assert_matches(traces[0].loop, """
        """)
