import py

from .base import BaseJITTest

class TestBasic(BaseJITTest):
    def test_while_loop(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        0 to: 1000000000 do: [:t|nil].
        """)
        self.assert_matches(traces[0].loop, """
         i59 = int_le(i51, 1000000000),
         guard_true(i59, descr=<Guard0x3336290>),
         i60 = int_add(i51, 1),
         i61 = int_sub(i55, 1),
         setfield_gc(ConstPtr(ptr52), i61, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         i62 = int_le(i61, 0),
         guard_false(i62, descr=<Guard0x3336250>),
         jump(p0, p3, i60, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i61, descr=TargetToken(53667152))
        """)
        self.assert_matches(traces[0].bridges[0], """

         f18 = call(ConstClass(ll_time.ll_time_time), descr=<Callf 8 EF=4>),
         setfield_gc(ConstPtr(ptr19), 10000, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         guard_no_exception(descr=<Guard0x2e964d0>),
         f22 = float_sub(f18, 1396948969.119000),
         f24 = float_mul(f22, 1000.000000),
         i25 = cast_float_to_int(f24),
         i27 = int_and(i25, 2147483647),
         i28 = getfield_gc(ConstPtr(ptr19), descr=<FieldS spyvm.interpreter.Interpreter.inst_next_wakeup_tick 36>),
         i29 = int_is_zero(i28),
         guard_true(i29, descr=<Guard0x2e96b50>),
         label(p0, p1, i16, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, descr=TargetToken(48874112)),
         guard_class(p0, 23085560, descr=<Guard0x2e96b10>),
         p31 = getfield_gc(p0, descr=<FieldP spyvm.shadow.ContextPartShadow.inst__w_method 44>),
         p32 = getfield_gc(p31, descr=<FieldP spyvm.model.W_CompiledMethod.inst_version 56>),
         guard_value(p31, ConstPtr(ptr33), descr=<Guard0x2e96ad0>),
         guard_value(p32, ConstPtr(ptr34), descr=<Guard0x2e96a90>),
         i36 = int_le(i16, 1000000000),
         guard_true(i36, descr=<Guard0x2e96a50>),
         i38 = int_add(i16, 1),
         setfield_gc(ConstPtr(ptr19), 9999, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         jump(p0, p1, i38, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, 9999, descr=TargetToken(48817488))
        """)

    def test_constant_string(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | i |
        i := 0.
        [i <= 10000] whileTrue: [ i := i + 'a' size].
        ^ i
        """)
        self.assert_matches(traces[0].loop, """
         i77 = int_le(i69, 10000),
         guard_true(i77, descr=<Guard0xfda8d0>),
         guard_not_invalidated(descr=<Guard0xfda890>),
         i78 = int_add_ovf(i69, i68),
         guard_no_overflow(descr=<Guard0xfda850>),
         i79 = int_sub(i72, 1),
         setfield_gc(ConstPtr(ptr66), i79, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         i80 = int_le(i79, 0),
         guard_false(i80, descr=<Guard0xfda810>),
         jump(p0, p3, i78, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i68, i79, descr=TargetToken(16561632))
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
         i79 = int_le(i71, 100000),
         guard_true(i79, descr=<Guard0x36e7790>),
         i80 = int_add(i71, 1),
         i81 = int_sub(i75, 1),
         setfield_gc(ConstPtr(ptr72), i81, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         i82 = int_le(i81, 0),
         guard_false(i82, descr=<Guard0x36e7c10>),
         i84 = arraylen_gc(p65, descr=<ArrayU 1>),
         i85 = arraylen_gc(p67, descr=<ArrayU 1>),
         jump(p0, p3, i80, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i81, p65, p67, descr=TargetToken(57534304))
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
         i72 = int_le(i64, 100000),
         guard_true(i72, descr=<Guard0x2e98590>),
         i73 = int_add(i64, 1),
         i74 = int_sub(i68, 1),
         setfield_gc(ConstPtr(ptr65), i74, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
         i75 = int_le(i74, 0),
         guard_false(i75, descr=<Guard0x2e98510>),
         jump(p0, p3, i73, p8, p10, p12, p14, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i74, descr=TargetToken(48821968))
        """)

    def test_bitblt_fillWhite(self, spy, tmpdir):
        # This used to have a call to array comparison in it
        traces = []
        retries = 10
        while len(traces) == 0 and retries > 0:
            retries -= 1
            traces = self.run(spy, tmpdir, """
            Display beDisplay. 1 to: 10000 do: [:i | Display fillWhite].
            """)
        self.assert_matches(traces[0].loop, """
            i598 = int_le(2, i153),
            guard_false(i598, descr=<Guard0x37cba50>),
            i599 = getfield_gc_pure(p589, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
            i600 = int_add_ovf(i599, i162),
            guard_no_overflow(descr=<Guard0x37cb910>),
            i601 = getfield_gc_pure(p592, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
            i602 = int_add_ovf(i601, i171),
            guard_no_overflow(descr=<Guard0x37cb7d0>),
            i603 = int_add_ovf(i176, 1),
            guard_no_overflow(descr=<Guard0x37cb710>),
            i604 = int_sub(i585, 1),
            setfield_gc(ConstPtr(ptr177), i604, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
            i605 = int_le(i604, 0),
            guard_false(i605, descr=<Guard0x37cb6d0>),
            i606 = int_le(i603, i187),
            guard_true(i606, descr=<Guard0x37cb3d0>),
            guard_not_invalidated(descr=<Guard0x37cb290>),
            i607 = getfield_gc_pure(p364, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
            i608 = int_mod(i607, i224),
            i609 = int_rshift(i608, 31),
            i610 = int_and(i224, i609),
            i611 = int_add(i608, i610),
            i612 = int_add_ovf(1, i611),
            guard_no_overflow(descr=<Guard0x37c8c90>),
            i613 = int_ge(i611, 0),
            guard_true(i613, descr=<Guard0x37c8a10>),
            i614 = int_lt(i611, i224),
            guard_true(i614, descr=<Guard0x37c89d0>),
            i615 = getarrayitem_gc(p247, i611, descr=<ArrayU 4>),
            i616 = uint_lt(i615, 0)
            guard_false(i616, descr=<Guard0x2f31410>)
            i617 = uint_lt(i615, 2147483647)
            guard_true(i617, descr=<Guard0x2f313d0>)
            i618 = int_add_ovf(i607, i256)
            guard_no_overflow(descr=<Guard0x2f31310>)
            i619 = int_ge(i615, 0)
            guard_true(i619, descr=<Guard0x2f2ef90>)
            i620 = int_and(i615, i615)
            i621 = uint_lt(i620, 2147483647)
            guard_true(i621, descr=<Guard0x2f2ef10>)
            i622 = int_add_ovf(i602, 1)
            guard_no_overflow(descr=<Guard0x2f2ee50>)
            i623 = int_ge(i602, 0)
            guard_true(i623, descr=<Guard0x2f2ea90>)
            i624 = int_lt(i602, i290)
            guard_true(i624, descr=<Guard0x2f2ea50>)
            i625 = getarrayitem_raw(i292, i602, descr=<ArrayU 4>)
            i626 = uint_lt(i625, 0)
            guard_false(i626, descr=<Guard0x2f2ea10>)
            i627 = uint_lt(i625, 2147483647)
            guard_true(i627, descr=<Guard0x2f2e9d0>)
            i628 = int_and(i328, i620)
            i629 = uint_lt(i628, 2147483647)
            guard_true(i629, descr=<Guard0x2f34850>)
            i630 = getarrayitem_raw(i292, i602, descr=<ArrayU 4>)
            i631 = uint_lt(i630, 0)
            guard_false(i631, descr=<Guard0x2f34c90>)
            i632 = uint_lt(i630, 2147483647)
            guard_true(i632, descr=<Guard0x2f34c50>)
            i633 = int_ge(i630, 0)
            guard_true(i633, descr=<Guard0x2f34c10>)
            i634 = int_and(i343, i630)
            i635 = uint_lt(i634, 2147483647)
            guard_true(i635, descr=<Guard0x2f34bd0>)
            i636 = int_ge(i628, 0)
            guard_true(i636, descr=<Guard0x2f34b90>)
            i637 = int_or(i628, i634)
            i638 = uint_lt(i637, 2147483647)
            guard_true(i638, descr=<Guard0x2f34b50>)
            setarrayitem_raw(i292, i602, i637, descr=<ArrayU 4>)
            i640 = int_lshift(i602, 3)
            i641 = int_ge(i640, i290)
            guard_false(i641, descr=<Guard0x2f34b10>)
            i642 = uint_rshift(i637, i386)
            i643 = int_lshift(i637, i373)
            i644 = uint_rshift(i643, i386)
            i645 = int_lshift(i644, 8)
            i646 = int_or(i642, i645)
            i647 = int_lshift(i643, i373)
            i648 = uint_rshift(i647, i386)
            i649 = int_lshift(i648, 16)
            i650 = int_or(i646, i649)
            i651 = int_lshift(i647, i373)
            i652 = uint_rshift(i651, i386)
            i653 = int_lshift(i652, 24)
            i654 = int_or(i650, i653)
            i655 = int_lshift(i651, i373)
            setarrayitem_raw(8650752, i640, i654, descr=<ArrayU 4>)
            i656 = int_add(i640, 1)
            i657 = int_ge(i656, i290)
            guard_false(i657, descr=<Guard0x2f34ad0>)
            i658 = uint_rshift(i655, i386)
            i659 = int_lshift(i655, i373)
            i660 = uint_rshift(i659, i386)
            i661 = int_lshift(i660, 8)
            i662 = int_or(i658, i661)
            i663 = int_lshift(i659, i373)
            i664 = uint_rshift(i663, i386)
            i665 = int_lshift(i664, 16)
            i666 = int_or(i662, i665)
            i667 = int_lshift(i663, i373)
            i668 = uint_rshift(i667, i386)
            i669 = int_lshift(i668, 24)
            i670 = int_or(i666, i669)
            i671 = int_lshift(i667, i373)
            setarrayitem_raw(8650752, i656, i670, descr=<ArrayU 4>)
            i672 = int_add(i656, 1)
            i673 = int_ge(i672, i290)
            guard_false(i673, descr=<Guard0x2f34a90>)
            i674 = uint_rshift(i671, i386)
            i675 = int_lshift(i671, i373)
            i676 = uint_rshift(i675, i386)
            i677 = int_lshift(i676, 8)
            i678 = int_or(i674, i677)
            i679 = int_lshift(i675, i373)
            i680 = uint_rshift(i679, i386)
            i681 = int_lshift(i680, 16)
            i682 = int_or(i678, i681)
            i683 = int_lshift(i679, i373)
            i684 = uint_rshift(i683, i386)
            i685 = int_lshift(i684, 24)
            i686 = int_or(i682, i685)
            i687 = int_lshift(i683, i373)
            setarrayitem_raw(8650752, i672, i686, descr=<ArrayU 4>)
            i688 = int_add(i672, 1)
            i689 = int_ge(i688, i290)
            guard_false(i689, descr=<Guard0x2f34a50>)
            i690 = uint_rshift(i687, i386)
            i691 = int_lshift(i687, i373)
            i692 = uint_rshift(i691, i386)
            i693 = int_lshift(i692, 8)
            i694 = int_or(i690, i693)
            i695 = int_lshift(i691, i373)
            i696 = uint_rshift(i695, i386)
            i697 = int_lshift(i696, 16)
            i698 = int_or(i694, i697)
            i699 = int_lshift(i695, i373)
            i700 = uint_rshift(i699, i386)
            i701 = int_lshift(i700, 24)
            i702 = int_or(i698, i701)
            i703 = int_lshift(i699, i373)
            setarrayitem_raw(8650752, i688, i702, descr=<ArrayU 4>)
            i704 = int_add(i688, 1)
            i705 = int_ge(i704, i290)
            guard_false(i705, descr=<Guard0x2f34a10>)
            i706 = uint_rshift(i703, i386)
            i707 = int_lshift(i703, i373)
            i708 = uint_rshift(i707, i386)
            i709 = int_lshift(i708, 8)
            i710 = int_or(i706, i709)
            i711 = int_lshift(i707, i373)
            i712 = uint_rshift(i711, i386)
            i713 = int_lshift(i712, 16)
            i714 = int_or(i710, i713)
            i715 = int_lshift(i711, i373)
            i716 = uint_rshift(i715, i386)
            i717 = int_lshift(i716, 24)
            i718 = int_or(i714, i717)
            i719 = int_lshift(i715, i373)
            setarrayitem_raw(8650752, i704, i718, descr=<ArrayU 4>)
            i720 = int_add(i704, 1)
            i721 = int_ge(i720, i290)
            guard_false(i721, descr=<Guard0x2f349d0>)
            i722 = uint_rshift(i719, i386)
            i723 = int_lshift(i719, i373)
            i724 = uint_rshift(i723, i386)
            i725 = int_lshift(i724, 8)
            i726 = int_or(i722, i725)
            i727 = int_lshift(i723, i373)
            i728 = uint_rshift(i727, i386)
            i729 = int_lshift(i728, 16)
            i730 = int_or(i726, i729)
            i731 = int_lshift(i727, i373)
            i732 = uint_rshift(i731, i386)
            i733 = int_lshift(i732, 24)
            i734 = int_or(i730, i733)
            i735 = int_lshift(i731, i373)
            setarrayitem_raw(8650752, i720, i734, descr=<ArrayU 4>)
            i736 = int_add(i720, 1)
            i737 = int_ge(i736, i290)
            guard_false(i737, descr=<Guard0x2f34990>)
            i738 = uint_rshift(i735, i386)
            i739 = int_lshift(i735, i373)
            i740 = uint_rshift(i739, i386)
            i741 = int_lshift(i740, 8)
            i742 = int_or(i738, i741)
            i743 = int_lshift(i739, i373)
            i744 = uint_rshift(i743, i386)
            i745 = int_lshift(i744, 16)
            i746 = int_or(i742, i745)
            i747 = int_lshift(i743, i373)
            i748 = uint_rshift(i747, i386)
            i749 = int_lshift(i748, 24)
            i750 = int_or(i746, i749)
            i751 = int_lshift(i747, i373)
            setarrayitem_raw(8650752, i736, i750, descr=<ArrayU 4>)
            i752 = int_add(i736, 1)
            i753 = int_ge(i752, i290)
            guard_false(i753, descr=<Guard0x2f34950>)
            i754 = uint_rshift(i751, i386)
            i755 = int_lshift(i751, i373)
            i756 = uint_rshift(i755, i386)
            i757 = int_lshift(i756, 8)
            i758 = int_or(i754, i757)
            i759 = int_lshift(i755, i373)
            i760 = uint_rshift(i759, i386)
            i761 = int_lshift(i760, 16)
            i762 = int_or(i758, i761)
            i763 = int_lshift(i759, i373)
            i764 = uint_rshift(i763, i386)
            i765 = int_lshift(i764, 24)
            i766 = int_or(i762, i765)
            i767 = int_lshift(i763, i373)
            setarrayitem_raw(8650752, i752, i766, descr=<ArrayU 4>)
            i768 = int_add(i752, 1)
            i769 = int_add_ovf(i600, i571)
            guard_no_overflow(descr=<Guard0x2f34910>)
            i770 = int_add_ovf(i602, i571)
            guard_no_overflow(descr=<Guard0x2f348d0>)
            i771 = int_sub(i604, 11)
            setfield_gc(ConstPtr(ptr177), i771, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i772 = int_le(i771, 0)
            guard_false(i772, descr=<Guard0x2f34890>)
            p773 = new_with_vtable(23083336)
            setfield_gc(p773, i769, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            setarrayitem_gc(p147, 34, p773, descr=<ArrayP 4>)
            p774 = new_with_vtable(23083336)
            setfield_gc(p774, i770, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            setarrayitem_gc(p147, 35, p774, descr=<ArrayP 4>)
            p775 = new_with_vtable(23083336)
            setfield_gc(p775, i618, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            setarrayitem_gc(p147, 20, p775, descr=<ArrayP 4>)
            i776 = arraylen_gc(p147, descr=<ArrayP 4>)
            i777 = arraylen_gc(p581, descr=<ArrayP 4>)
            jump(p0, p3, p8, i615, p596, i620, p18, i603, p38, p40, p42, p44, p46, p48, p50, p52, p54, p56, p58, p60, p62, p64, p66, p68, p70, p72, p74, p76, p78, p80, p82, p84, p86, p88, p90, p92, p94, p96, p98, p100, p102, p104, p106, p108, p110, p112, p114, p116, p118, p120, p122, p124, p126, p128, p130, p132, p134, 1, p149, p773, i162, p158, p774, i171, p167, p147, i771, i187, p184, p190, p775, i224, p200, p247, i256, p254, p263, p142, p281, i290, i292, i328, i343, i386, i373, i571, p569, p596, p581, descr=TargetToken(48932608))
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
