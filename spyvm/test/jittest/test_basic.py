import py

from .base import BaseJITTest

class TestBasic(BaseJITTest):
    def test_while_loop(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        0 to: 1000000000 do: [:t|nil].
        """)
        self.assert_matches(traces[0].loop, """
         guard_not_invalidated(descr=<Guard0x2e9b950>),
         i57 = int_le(i50, 1000000000),
         guard_true(i57, descr=<Guard0x2e9b910>),
         i58 = int_add(i50, 1),
         i59 = int_sub(i54, 1),
         setfield_gc(ConstPtr(ptr51), i59, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         i60 = int_le(i59, 0),
         guard_false(i60, descr=<Guard0x2e9b8d0>),
         jump(p0, p3, i58, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i43, i59, descr=TargetToken(48805440))
        """)
        self.assert_matches(traces[0].bridges[0], """
         f18 = call(ConstClass(ll_time.ll_time_time), descr=<Callf 8 EF=4>),
         setfield_gc(ConstPtr(ptr19), 10000, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         guard_no_exception(descr=<Guard0x381bb50>),
         f22 = float_sub(f18, 1395138051.488000),
         f24 = float_mul(f22, 1000.000000),
         i25 = cast_float_to_int(f24),
         i27 = int_and(i25, 2147483647),
         i28 = getfield_gc(ConstPtr(ptr19), descr=<FieldS spyvm.interpreter.Interpreter.inst_next_wakeup_tick 36>),
         i29 = int_is_zero(i28),
         guard_true(i29, descr=<Guard0x3826ad0>),
         label(p0, p1, i16, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, descr=TargetToken(58860256)),
         guard_class(p0, ConstClass(MethodContextShadow), descr=<Guard0x3826a90>),
         p31 = getfield_gc(p0, descr=<FieldP spyvm.shadow.MethodContextShadow.inst__w_method 44>),
         guard_value(p31, ConstPtr(ptr32), descr=<Guard0x3826a50>),
         i33 = getfield_gc_pure(p0, descr=<FieldU spyvm.shadow.AbstractShadow.inst_space 12>),
         guard_not_invalidated(descr=<Guard0x3826a10>),
         i35 = int_le(i16, 1000000000),
         guard_true(i35, descr=<Guard0x38269d0>),
         i37 = int_add(i16, 1),
         setfield_gc(ConstPtr(ptr19), 9999, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         jump(p0, p1, i37, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, i33, 9999, descr=TargetToken(58766912))
        """)

    def test_constant_string(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | i |
        i := 0.
        [i <= 10000] whileTrue: [ i := i + 'a' size].
        ^ i
        """)
        self.assert_matches(traces[0].loop, """
         guard_not_invalidated(descr=<Guard0x34ccf10>),
         i61 = int_le(i55, 10000),
         guard_true(i61, descr=<Guard0x34cced0>),
         i62 = int_add(i55, 1),
         i63 = int_sub(i58, 1),
         setfield_gc(ConstPtr(ptr52), i63, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         i64 = int_le(i63, 0),
         guard_false(i64, descr=<Guard0x34cce90>),
         jump(p0, p3, i62, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i43, i63, descr=TargetToken(55305792))
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
         guard_not_invalidated(descr=<Guard0x2e222d0>),
         i65 = int_le(i58, 100000),
         guard_true(i65, descr=<Guard0x2e28e10>),
         i66 = int_add(i58, 1),
         i67 = int_sub(i62, 2),
         setfield_gc(ConstPtr(ptr59), i67, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         i68 = int_le(i67, 0),
         guard_false(i68, descr=<Guard0x2e28e50>),
         i70 = arraylen_gc(p50, descr=<ArrayU 1>),
         i71 = arraylen_gc(p54, descr=<ArrayU 1>),
         jump(p0, p3, i66, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i43, i67, p50, p54, descr=TargetToken(48301680))
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
        jump(p0, p3, i74, p8, p10, xx, p12, p14, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i77, descr=TargetToken(157713840))
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
            guard_not_invalidated(descr=<Guard0x3354f50>),
            i584 = int_le(2, i151),
            guard_false(i584, descr=<Guard0x33549d0>),
            i585 = getfield_gc_pure(p576, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
            i586 = int_add_ovf(i585, i160),
            guard_no_overflow(descr=<Guard0x3354350>),
            i587 = getfield_gc_pure(p579, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
            i588 = int_add_ovf(i587, i169),
            guard_no_overflow(descr=<Guard0x3351c90>),
            i589 = int_add_ovf(i174, 1),
            guard_no_overflow(descr=<Guard0x3351810>),
            i590 = int_sub(i572, 2),
            setfield_gc(ConstPtr(ptr175), i590, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
            i591 = int_le(i590, 0),
            guard_false(i591, descr=<Guard0x3351590>),
            i592 = int_le(i589, i185),
            guard_true(i592, descr=<Guard0x334eed0>),
            i593 = getfield_gc_pure(p350, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
            i594 = int_mod(i593, i219),
            i595 = int_rshift(i594, 31),
            i596 = int_and(i219, i595),
            i597 = int_add(i594, i596),
            i598 = int_add_ovf(1, i597),
            guard_no_overflow(descr=<Guard0x334ca10>),
            i599 = int_ge(i597, 0),
            guard_true(i599, descr=<Guard0x334c650>),
            i600 = int_lt(i597, i219),
            guard_true(i600, descr=<Guard0x334c610>),
            i601 = getarrayitem_gc(p241, i597, descr=<ArrayU 4>),
            i602 = uint_lt(i601, 0),
            guard_false(i602, descr=<Guard0x334c590>),
            i603 = uint_lt(i601, 2147483647),
            guard_true(i603, descr=<Guard0x334c550>),
            i604 = int_add_ovf(i593, i250),
            guard_no_overflow(descr=<Guard0x334c010>),
            i605 = int_ge(i601, 0),
            guard_true(i605, descr=<Guard0x33481d0>),
            i606 = int_and(i601, i601),
            i607 = uint_lt(i606, 2147483647),
            guard_true(i607, descr=<Guard0x3348150>),
            i608 = int_add_ovf(i588, 1),
            guard_no_overflow(descr=<Guard0x3344bd0>),
            i609 = int_ge(i588, 0),
            guard_true(i609, descr=<Guard0x33446d0>),
            i610 = int_lt(i588, i281),
            guard_true(i610, descr=<Guard0x3344690>),
            i611 = getarrayitem_raw(i283, i588, descr=<ArrayU 4>),
            i612 = uint_lt(i611, 0),
            guard_false(i612, descr=<Guard0x3344650>),
            i613 = uint_lt(i611, 2147483647),
            guard_true(i613, descr=<Guard0x3344610>),
            i614 = int_and(i318, i606),
            i615 = uint_lt(i614, 2147483647),
            guard_true(i615, descr=<Guard0x333da90>),
            i616 = getarrayitem_raw(i283, i588, descr=<ArrayU 4>),
            i617 = uint_lt(i616, 0),
            guard_false(i617, descr=<Guard0x33574d0>),
            i618 = uint_lt(i616, 2147483647),
            guard_true(i618, descr=<Guard0x3357490>),
            i619 = int_ge(i616, 0),
            guard_true(i619, descr=<Guard0x3357450>),
            i620 = int_and(i329, i616),
            i621 = uint_lt(i620, 2147483647),
            guard_true(i621, descr=<Guard0x3357410>),
            i622 = int_ge(i614, 0),
            guard_true(i622, descr=<Guard0x33573d0>),
            i623 = int_or(i614, i620),
            i624 = uint_lt(i623, 2147483647),
            guard_true(i624, descr=<Guard0x3357390>),
            setarrayitem_raw(i283, i588, i623, descr=<ArrayU 4>),
            i626 = int_lshift(i588, 3),
            i627 = int_ge(i626, i281),
            guard_false(i627, descr=<Guard0x3357350>),
            i628 = uint_rshift(i623, i373),
            i629 = int_lshift(i623, i360),
            i630 = uint_rshift(i629, i373),
            i631 = int_lshift(i630, 8),
            i632 = int_or(i628, i631),
            i633 = int_lshift(i629, i360),
            i634 = uint_rshift(i633, i373),
            i635 = int_lshift(i634, 16),
            i636 = int_or(i632, i635),
            i637 = int_lshift(i633, i360),
            i638 = uint_rshift(i637, i373),
            i639 = int_lshift(i638, 24),
            i640 = int_or(i636, i639),
            i641 = int_lshift(i637, i360),
            setarrayitem_raw(51118152, i626, i640, descr=<ArrayU 4>),
            i642 = int_add(i626, 1),
            i643 = int_ge(i642, i281),
            guard_false(i643, descr=<Guard0x3357310>),
            i644 = uint_rshift(i641, i373),
            i645 = int_lshift(i641, i360),
            i646 = uint_rshift(i645, i373),
            i647 = int_lshift(i646, 8),
            i648 = int_or(i644, i647),
            i649 = int_lshift(i645, i360),
            i650 = uint_rshift(i649, i373),
            i651 = int_lshift(i650, 16),
            i652 = int_or(i648, i651),
            i653 = int_lshift(i649, i360),
            i654 = uint_rshift(i653, i373),
            i655 = int_lshift(i654, 24),
            i656 = int_or(i652, i655),
            i657 = int_lshift(i653, i360),
            setarrayitem_raw(51118152, i642, i656, descr=<ArrayU 4>),
            i658 = int_add(i642, 1),
            i659 = int_ge(i658, i281),
            guard_false(i659, descr=<Guard0x33572d0>),
            i660 = uint_rshift(i657, i373),
            i661 = int_lshift(i657, i360),
            i662 = uint_rshift(i661, i373),
            i663 = int_lshift(i662, 8),
            i664 = int_or(i660, i663),
            i665 = int_lshift(i661, i360),
            i666 = uint_rshift(i665, i373),
            i667 = int_lshift(i666, 16),
            i668 = int_or(i664, i667),
            i669 = int_lshift(i665, i360),
            i670 = uint_rshift(i669, i373),
            i671 = int_lshift(i670, 24),
            i672 = int_or(i668, i671),
            i673 = int_lshift(i669, i360),
            setarrayitem_raw(51118152, i658, i672, descr=<ArrayU 4>),
            i674 = int_add(i658, 1),
            i675 = int_ge(i674, i281),
            guard_false(i675, descr=<Guard0x3357290>),
            i676 = uint_rshift(i673, i373),
            i677 = int_lshift(i673, i360),
            i678 = uint_rshift(i677, i373),
            i679 = int_lshift(i678, 8),
            i680 = int_or(i676, i679),
            i681 = int_lshift(i677, i360),
            i682 = uint_rshift(i681, i373),
            i683 = int_lshift(i682, 16),
            i684 = int_or(i680, i683),
            i685 = int_lshift(i681, i360),
            i686 = uint_rshift(i685, i373),
            i687 = int_lshift(i686, 24),
            i688 = int_or(i684, i687),
            i689 = int_lshift(i685, i360),
            setarrayitem_raw(51118152, i674, i688, descr=<ArrayU 4>),
            i690 = int_add(i674, 1),
            i691 = int_ge(i690, i281),
            guard_false(i691, descr=<Guard0x3357250>),
            i692 = uint_rshift(i689, i373),
            i693 = int_lshift(i689, i360),
            i694 = uint_rshift(i693, i373),
            i695 = int_lshift(i694, 8),
            i696 = int_or(i692, i695),
            i697 = int_lshift(i693, i360),
            i698 = uint_rshift(i697, i373),
            i699 = int_lshift(i698, 16),
            i700 = int_or(i696, i699),
            i701 = int_lshift(i697, i360),
            i702 = uint_rshift(i701, i373),
            i703 = int_lshift(i702, 24),
            i704 = int_or(i700, i703),
            i705 = int_lshift(i701, i360),
            setarrayitem_raw(51118152, i690, i704, descr=<ArrayU 4>),
            i706 = int_add(i690, 1),
            i707 = int_ge(i706, i281),
            guard_false(i707, descr=<Guard0x3357210>),
            i708 = uint_rshift(i705, i373),
            i709 = int_lshift(i705, i360),
            i710 = uint_rshift(i709, i373),
            i711 = int_lshift(i710, 8),
            i712 = int_or(i708, i711),
            i713 = int_lshift(i709, i360),
            i714 = uint_rshift(i713, i373),
            i715 = int_lshift(i714, 16),
            i716 = int_or(i712, i715),
            i717 = int_lshift(i713, i360),
            i718 = uint_rshift(i717, i373),
            i719 = int_lshift(i718, 24),
            i720 = int_or(i716, i719),
            i721 = int_lshift(i717, i360),
            setarrayitem_raw(51118152, i706, i720, descr=<ArrayU 4>),
            i722 = int_add(i706, 1),
            i723 = int_ge(i722, i281),
            guard_false(i723, descr=<Guard0x33571d0>),
            i724 = uint_rshift(i721, i373),
            i725 = int_lshift(i721, i360),
            i726 = uint_rshift(i725, i373),
            i727 = int_lshift(i726, 8),
            i728 = int_or(i724, i727),
            i729 = int_lshift(i725, i360),
            i730 = uint_rshift(i729, i373),
            i731 = int_lshift(i730, 16),
            i732 = int_or(i728, i731),
            i733 = int_lshift(i729, i360),
            i734 = uint_rshift(i733, i373),
            i735 = int_lshift(i734, 24),
            i736 = int_or(i732, i735),
            i737 = int_lshift(i733, i360),
            setarrayitem_raw(51118152, i722, i736, descr=<ArrayU 4>),
            i738 = int_add(i722, 1),
            i739 = int_ge(i738, i281),
            guard_false(i739, descr=<Guard0x3357190>),
            i740 = uint_rshift(i737, i373),
            i741 = int_lshift(i737, i360),
            i742 = uint_rshift(i741, i373),
            i743 = int_lshift(i742, 8),
            i744 = int_or(i740, i743),
            i745 = int_lshift(i741, i360),
            i746 = uint_rshift(i745, i373),
            i747 = int_lshift(i746, 16),
            i748 = int_or(i744, i747),
            i749 = int_lshift(i745, i360),
            i750 = uint_rshift(i749, i373),
            i751 = int_lshift(i750, 24),
            i752 = int_or(i748, i751),
            i753 = int_lshift(i749, i360),
            setarrayitem_raw(51118152, i738, i752, descr=<ArrayU 4>),
            i754 = int_add(i738, 1),
            i755 = int_add_ovf(i586, i558),
            guard_no_overflow(descr=<Guard0x3357150>),
            i756 = int_add_ovf(i588, i558),
            guard_no_overflow(descr=<Guard0x3357110>),
            i757 = int_sub(i590, 23),
            setfield_gc(ConstPtr(ptr175), i757, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
            i758 = int_le(i757, 0),
            guard_false(i758, descr=<Guard0x33570d0>),
            p759 = new_with_vtable(18295080),
            setfield_gc(p759, i755, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
            setarrayitem_gc(p145, 34, p759, descr=<ArrayP 4>),
            p760 = new_with_vtable(18295080),
            setfield_gc(p760, i756, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
            setarrayitem_gc(p145, 35, p760, descr=<ArrayP 4>),
            p761 = new_with_vtable(18295080),
            setfield_gc(p761, i604, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
            setarrayitem_gc(p145, 20, p761, descr=<ArrayP 4>),
            i762 = arraylen_gc(p145, descr=<ArrayP 4>),
            i763 = arraylen_gc(p568, descr=<ArrayP 4>),
            jump(p0, p3, p8, i601, p582, i606, p18, i589, p38, p40, p42, p44, p46, p48, p50, p52, p54, p56, p58, p60, p62, p64, p66, p68, p70, p72, p74, p76, p78, p80, p82, p84, p86, p88, p90, p92, p94, p96, p98, p100, p102, p104, p106, p108, p110, p112, p114, p116, p118, p120, p122, p124, p126, p128, p130, p132, p134, i139, 1, p147, p759, i160, p156, p760, i169, p165, p145, i757, i185, p182, p761, i219, p195, p241, i250, p248, p257, p141, p272, i281, i283, i318, i329, i373, i360, i558, p556, p582, p568, descr=TargetToken(53262992))
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
