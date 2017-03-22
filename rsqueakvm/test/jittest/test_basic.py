import py

from .base import BaseJITTest

from rsqueakvm.util.system import IS_64BIT

class TestBasic(BaseJITTest):
    def test_while_loop(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        0 to: 10000 do: [:t|nil].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x910a1f0>)
        i59 = int_le(i51, 1000000000),
        guard_true(i59, descr=<Guard0x3336290>),
        i60 = int_add(i51, 1),
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i73 = int_le(i70, 0),
        guard_false(i73, descr=<Guard0x9c13130>),
        jump(p0, p3, i60, p12, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, i61, descr=TargetToken(53667152))
        """)

    def test_constant_string(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | i |
        i := 0.
        [i <= 10000] whileTrue: [ i := i + 'a' size].
        ^ i
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x8e3dc04>)
        i83 = int_le(i77, 10000),
        guard_true(i83, descr=<Guard0x8e3dc40>),
        i85 = int_add_ovf(i77, 1),
        guard_no_overflow(descr=<Guard0xa6db2cc>)
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i73 = int_le(i70, 0),
        guard_false(i73, descr=<Guard0x9c13130>),
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, i85, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, p61, i86, descr=TargetToken(149146648))]
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
        guard_not_invalidated(descr=<Guard0x9730760>)
        i71 = int_le(i64, 100000),
        guard_true(i71, descr=<Guard0x9730790>),
        i72 = int_add(i64, 1),
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i73 = int_le(i70, 0),
        guard_false(i73, descr=<Guard0x9c13130>),
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, i72, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i73, descr=TargetToken(158683952))
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
        guard_not_invalidated(descr=<Guard0x9730760>)
        i72 = int_le(i64, 100000),
        guard_true(i72, descr=<Guard0x2e98590>),
        i73 = int_add(i64, 1),
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i74 = int_le(i70, 0),
        guard_false(i74, descr=<Guard0x9c13130>),
        jump(p0, p3, i73, p8, p10, p12, p14, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i74, descr=TargetToken(48821968))
        """)

    @py.test.mark.skipif("'not working'")
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
            i598 = int_le(2, i153)
            guard_false(i598, descr=<Guard0x37cba50>),
            i599 = getfield_gc_pure(p589, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8>),
            i600 = int_add_ovf(i599, i162),
            guard_no_overflow(descr=<Guard0x37cb910>),
            i601 = getfield_gc_pure(p592, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8>),
            i602 = int_add_ovf(i601, i171),
            guard_no_overflow(descr=<Guard0x37cb7d0>),
            i603 = int_add_ovf(i176, 1),
            guard_no_overflow(descr=<Guard0x37cb710>),
            i604 = int_sub(i585, 1),
            setfield_gc(ConstPtr(ptr177), i604, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
            i605 = int_le(i604, 0),
            guard_false(i605, descr=<Guard0x37cb6d0>),
            i606 = int_le(i603, i187),
            guard_true(i606, descr=<Guard0x37cb3d0>),
            guard_not_invalidated(descr=<Guard0x37cb290>),
            i607 = getfield_gc_pure(p364, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8>),
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
            setfield_gc(ConstPtr(ptr177), i771, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i772 = int_le(i771, 0)
            guard_false(i772, descr=<Guard0x2f34890>)
            p773 = new_with_vtable(23083336)
            setfield_gc(p773, i769, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8>)
            setarrayitem_gc(p147, 34, p773, descr=<ArrayP 4>)
            p774 = new_with_vtable(23083336)
            setfield_gc(p774, i770, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8>)
            setarrayitem_gc(p147, 35, p774, descr=<ArrayP 4>)
            p775 = new_with_vtable(23083336)
            setfield_gc(p775, i618, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8>)
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

    def test_bytecode_prim_add(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | p |
        p := 1.
        1 to: 10000 do: [:i | p + p].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x9730760>)
        i68 = int_le(i61, 10000),
        guard_true(i68, descr=<Guard0x916d700>),
        i69 = int_add(i61, 1),
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i73 = int_le(i70, 0),
        guard_false(i73, descr=<Guard0x9c13130>),
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, p16, i69, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i70, descr=TargetToken(152642232))
        """)

    def test_bytecode_prim_add_fail(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | p |
        p := 1@2.
        1 to: 10000 do: [:i | p + p].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x93168f8>)
        i129 = int_le(i123, 10000),
        guard_true(i129, descr=<Guard0x9316934>),
        p98 = force_token()
        enter_portal_frame(0, 0)
        leave_portal_frame(0),
        i141 = int_add(i123, 1),
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i73 = int_le(i70, 0),
        guard_false(i73, descr=<Guard0x9c13130>),
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, p16, i141, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, p62, p84, i142, p114, descr=TargetToken(154312720))]
        """)

    def test_large_integer_add(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | li block |
        li := 2 raisedTo: 32 - 1.
        1 to: 1000000 do: [:i | li + i].
        """)
        if IS_64BIT:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x21a53a0>)
            i69 = int_le(i60, 1000000)
            guard_true(i69, descr=<Guard0x21feba8>)
            i70 = int_add_ovf(i57, i60)
            guard_no_overflow(descr=<Guard0x21febf0>)
            i72 = int_add(i60, 1)
            i74 = int_sub(i64, 1)
            setfield_gc(ConstPtr(ptr75), i74, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i77 = int_le(i74, 0)
            guard_false(i77, descr=<Guard0x21a5408>)
            jump(p0, p3, p4, i5, p6, p8, p9, p10, p13, p15, i72, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i57, i74, descr=TargetToken(37966400))
            """)
        else:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x343f5c0>)
            i105 = int_le(i96, 1000000)
            guard_true(i105, descr=<Guard0x3480760>)
            f107 = call_f(ConstClass(_ll_1_llong_from_int__Signed), i96, descr=<CallL 8 i EF=0 OS=84>)
            f109 = call_f(ConstClass(_ll_2_llong_add__SignedLongLong_SignedLongLong), f57, f107, descr=<CallL 8 LL EF=0 OS=70>)
            f111 = call_f(ConstClass(_ll_2_llong_xor__SignedLongLong_SignedLongLong), f109, f57, descr=<CallL 8 LL EF=0 OS=83>)
            i114 = call_i(ConstClass(_ll_2_llong_lt__SignedLongLong_SignedLongLong), f111, 0.000000, descr=<Calli 1 LL EF=0 OS=73>)
            f116 = call_f(ConstClass(_ll_2_llong_xor__SignedLongLong_SignedLongLong), f109, f107, descr=<CallL 8 LL EF=0 OS=83>)
            i119 = call_i(ConstClass(_ll_2_llong_lt__SignedLongLong_SignedLongLong), f116, 0.000000, descr=<Calli 1 LL EF=0 OS=73>)
            i120 = int_and(i114, i119)
            i121 = int_is_true(i120)
            guard_false(i121, descr=<Guard0x343f5f4>)
            i124 = call_i(ConstClass(_ll_2_llong_le__SignedLongLong_SignedLongLong), nan, f109, descr=<Calli 1 LL EF=0 OS=74>)
            i127 = call_i(ConstClass(_ll_2_llong_le__SignedLongLong_SignedLongLong), f109, 0.000000, descr=<Calli 1 LL EF=0 OS=74>)
            i128 = int_and(i124, i127)
            i129 = int_is_true(i128)
            guard_false(i129, descr=<Guard0x343f628>)
            i132 = call_i(ConstClass(_ll_2_llong_le__SignedLongLong_SignedLongLong), 0.000000, f109, descr=<Calli 1 LL EF=0 OS=74>)
            i135 = call_i(ConstClass(_ll_2_llong_le__SignedLongLong_SignedLongLong), f109, 0.000000, descr=<Calli 1 LL EF=0 OS=74>)
            i136 = int_and(i132, i135)
            i137 = int_is_true(i136)
            guard_true(i137, descr=<Guard0x343f65c>)
            i139 = call_i(ConstClass(_ll_1_llong_to_int__SignedLongLong), f109, descr=<Calli 4 L EF=0 OS=85>)
            i141 = int_add(i96, 1)
            i143 = int_sub(i100, 1)
            setfield_gc(ConstPtr(ptr144), i143, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>)
            i146 = int_le(i143, 0)
            guard_false(i146, descr=<Guard0x343f690>)
            jump(p0, p1, i2, p3, p4, p7, p8, p10, p13, p15, i141, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i55, f57, i143, descr=TargetToken(55003392))
            """)

    def test_large_integer_long_div(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | li block |
        li := 8589934592.
        1 to: 10000 do: [:i | li // i].
        """)
        if not IS_64BIT:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x346e5f4>)
            i100 = int_le(i91, 10000)
            guard_true(i100, descr=<Guard0x34af7cc>)
            i102 = call_i(ConstClass(rbigint._toint_helper), p55, descr=<Calli 4 r EF=4>)
            p104 = guard_exception(ConstClass(OverflowError), descr=<Guard0x346e628>)
            f106 = call_f(ConstClass(rbigint.tolonglong), p55, descr=<CallL 8 r EF=4>)
            guard_no_exception(descr=<Guard0x346e65c>)
            f108 = call_f(ConstClass(_ll_1_llong_from_int__Signed), i91, descr=<CallL 8 i EF=0 OS=84>)
            i111 = call_i(ConstClass(_ll_2_llong_eq__SignedLongLong_SignedLongLong), f108, 0.000000, descr=<Calli 1 LL EF=0 OS=75>)
            guard_false(i111, descr=<Guard0x346e690>)
            i114 = call_i(ConstClass(_ll_2_llong_eq__SignedLongLong_SignedLongLong), f106, nan, descr=<Calli 1 LL EF=0 OS=75>)
            i117 = call_i(ConstClass(_ll_2_llong_eq__SignedLongLong_SignedLongLong), f108, nan, descr=<Calli 1 LL EF=0 OS=75>)
            i118 = int_and(i114, i117)
            i119 = int_is_true(i118)
            guard_false(i119, descr=<Guard0x346e6c4>)
            f121 = call_f(ConstClass(ll_llong_py_div__SignedLongLong_SignedLongLong), f106, f108, descr=<CallL 8 LL EF=2>)
            i124 = call_i(ConstClass(_ll_2_llong_le__SignedLongLong_SignedLongLong), nan, f121, descr=<Calli 1 LL EF=0 OS=74>)
            i127 = call_i(ConstClass(_ll_2_llong_le__SignedLongLong_SignedLongLong), f121, 0.000000, descr=<Calli 1 LL EF=0 OS=74>)
            i128 = int_and(i124, i127)
            i129 = int_is_true(i128)
            guard_true(i129, descr=<Guard0x346e6f8>)
            i131 = call_i(ConstClass(_ll_1_llong_to_int__SignedLongLong), f121, descr=<Calli 4 L EF=0 OS=85>)
            i133 = int_add(i91, 1)
            i135 = int_sub(i95, 1)
            setfield_gc(ConstPtr(ptr136), i135, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>)
            i138 = int_le(i135, 0)
            guard_false(i138, descr=<Guard0x346e72c>)
            jump(p0, p1, i2, p3, p4, p7, p8, p10, p13, p15, i133, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p55, i135, descr=TargetToken(55196036))
            """)
        else:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x7f81bd321dc8>)
            i72 = int_le(i63, 10000)
            guard_true(i72, descr=<Guard0x19f6260>)
            i74 = int_eq(i63, 0)
            guard_false(i74, descr=<Guard0x19f62a8>)
            i76 = int_eq(i63, -1)
            i77 = int_and(i56, i76)
            guard_false(i77, descr=<Guard0x19f62f0>)
            i79 = call_i(ConstClass(ll_int_py_div__Signed_Signed), i52, i63, descr=<Calli 8 ii EF=0 OS=12>)
            i81 = int_add(i63, 1)
            i83 = int_sub(i67, 1)
            setfield_gc(ConstPtr(ptr84), i83, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i86 = int_le(i83, 0)
            guard_false(i86, descr=<Guard0x7f81bd321e30>)
            jump(p0, p1, i2, p4, p5, p6, p8, p11, p13, i81, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i56, i52, i83, descr=TargetToken(29952464))
            """)

    def test_large_integer_div(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | li block |
        li := 2 raisedTo: 32 - 1.
        1 to: 100000 do: [:i | li // i].
        """)
        if not IS_64BIT:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x353f5c0>)
            i96 = int_le(i87, 100000)
            guard_true(i96, descr=<Guard0x358073c>)
            f98 = call_f(ConstClass(_ll_1_llong_from_int__Signed), i87, descr=<CallL 8 i EF=0 OS=84>)
            i101 = call_i(ConstClass(_ll_2_llong_eq__SignedLongLong_SignedLongLong), f98, 0.000000, descr=<Calli 1 LL EF=0 OS=75>)
            guard_false(i101, descr=<Guard0x353f5f4>)
            i104 = call_i(ConstClass(_ll_2_llong_eq__SignedLongLong_SignedLongLong), f98, nan, descr=<Calli 1 LL EF=0 OS=75>)
            i105 = int_and(i68, i104)
            i106 = int_is_true(i105)
            guard_false(i106, descr=<Guard0x353f628>)
            f108 = call_f(ConstClass(ll_llong_py_div__SignedLongLong_SignedLongLong), f57, f98, descr=<CallL 8 LL EF=2>)
            i111 = call_i(ConstClass(_ll_2_llong_le__SignedLongLong_SignedLongLong), nan, f108, descr=<Calli 1 LL EF=0 OS=74>)
            i114 = call_i(ConstClass(_ll_2_llong_le__SignedLongLong_SignedLongLong), f108, 0.000000, descr=<Calli 1 LL EF=0 OS=74>)
            i115 = int_and(i111, i114)
            i116 = int_is_true(i115)
            guard_true(i116, descr=<Guard0x353f65c>)
            i118 = call_i(ConstClass(_ll_1_llong_to_int__SignedLongLong), f108, descr=<Calli 4 L EF=0 OS=85>)
            i120 = int_add(i87, 1)
            i122 = int_sub(i91, 1)
            setfield_gc(ConstPtr(ptr123), i122, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>)
            i125 = int_le(i122, 0)
            guard_false(i125, descr=<Guard0x353f690>)
            jump(p0, p1, i2, p3, p4, p7, p8, p10, p13, p15, i120, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i55, f57, i68, i122, descr=TargetToken(56051968))
            """)
        else:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x7fccf8891dc8>)
            i72 = int_le(i63, 100000)
            guard_true(i72, descr=<Guard0x1f6a188>)
            i74 = int_eq(i63, 0)
            guard_false(i74, descr=<Guard0x1f6a1d0>)
            i76 = int_eq(i63, -1)
            i77 = int_and(i56, i76)
            guard_false(i77, descr=<Guard0x1f6a218>)
            i79 = call_i(ConstClass(ll_int_py_div__Signed_Signed), i52, i63, descr=<Calli 8 ii EF=0 OS=12>)
            i81 = int_add(i63, 1)
            i83 = int_sub(i67, 1)
            setfield_gc(ConstPtr(ptr84), i83, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i86 = int_le(i83, 0)
            guard_false(i86, descr=<Guard0x7fccf8891e30>)
            jump(p0, p1, i2, p4, p5, p6, p8, p11, p13, i81, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, i56, i52, i83, descr=TargetToken(35670240))
            """)

    def test_large_integer_xor(self, spy, tmpdir):
        if IS_64BIT: return # XXX
        traces = self.run(spy, tmpdir, """
        | li block |
        li := 2147483648.
        1 to: 100000 do: [:i | li bitXorLarge: i].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xac75cd0>)
        i97 = int_le(i90, 100000),
        guard_true(i97, descr=<Guard0xac75ca0>),
        i100 = int_xor(i83, i90),
        i101 = uint_le(i100, 2147483647),
        guard_false(i101, descr=<Guard0xac75c10>),
        i102 = int_add(i90, 1),
        i70 = int_sub(i61, 1),
        setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
        i73 = int_le(i70, 0),
        guard_false(i73, descr=<Guard0x9c13130>),
        jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, p16, p18, i102, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, p65, i83, i103, p79, descr=TargetToken(181116944))
        """)

    def test_large_integer_and(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | li block |
        li := 2147483648.
        1 to: 100000 do: [:i | li bitAnd: i].
        """)
        if not IS_64BIT:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0xa11ad30>)
            i72 = int_le(i65, 100000),
            guard_true(i72, descr=<Guard0xa11ad00>),
            i73 = int_and(i58, i65),
            i74 = uint_le(i73, 2147483647),
            guard_true(i74, descr=<Guard0xa11acd0>),
            i75 = int_add(i65, 1),
            i70 = int_sub(i61, 1),
            setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
            i78 = int_le(i70, 0),
            guard_false(i78, descr=<Guard0x9c13130>),
            jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, p16, p18, i75, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i58, i76, descr=TargetToken(169079000))
            """)
        else:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x1f113a0>)
            i69 = int_le(i60, 100000)
            guard_true(i69, descr=<Guard0x1f52b18>)
            i70 = int_and(i57, i60)
            i72 = int_add(i60, 1)
            i74 = int_sub(i64, 1)
            setfield_gc(ConstPtr(ptr75), i74, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i77 = int_le(i74, 0)
            guard_false(i77, descr=<Guard0x1f11408>)
            jump(p0, p3, p4, i5, p6, p8, p9, p10, p13, p15, i72, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i57, i74, descr=TargetToken(35156544))
            """)

    def test_large_integer_or(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | li block |
        li := 2147483648.
        1 to: 100000 do: [:i | li bitOr: i].
        """)
        if not IS_64BIT:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0xa11ad30>)
            i72 = int_le(i65, 100000),
            guard_true(i72, descr=<Guard0xa11ad00>),
            i73 = int_or(i58, i65),
            i74 = uint_le(i73, 2147483647),
            guard_false(i74, descr=<Guard0xa11acd0>),
            i75 = int_add(i65, 1),
            i70 = int_sub(i61, 1),
            setfield_gc(ConstPtr(ptr71), i70, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>),
            i78 = int_le(i70, 0),
            guard_false(i78, descr=<Guard0x9c13130>),
            jump(p0, p3, p4, i5, i6, p7, i8, i9, p11, p12, p13, p16, p18, i75, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, i58, i76, descr=TargetToken(169079000))
            """)
        else:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x2bf73a0>)
            i69 = int_le(i60, 100000)
            guard_true(i69, descr=<Guard0x2c38b18>)
            i70 = int_or(i57, i60)
            i72 = int_add(i60, 1)
            i74 = int_sub(i64, 1)
            setfield_gc(ConstPtr(ptr75), i74, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i77 = int_le(i74, 0)
            guard_false(i77, descr=<Guard0x2bf7408>)
            jump(p0, p3, p4, i5, p6, p8, p9, p10, p13, p15, i72, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i57, i74, descr=TargetToken(48681536))
            """)

    def test_object_access(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | o |
        o := Array with: (Array with: 'a' with: $b) with: (Array with: 'x' with: $y).
        1 to: 10000 do: [:i | (o at: (i \\\\ 2) + 1) at: (i \\\\ 2) + 1].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0xab945c0>)
        i94 = int_le(i85, 10000)
        guard_true(i94, descr=<Guard0xabbbc70>)
        i96 = int_eq(i85, -9223372036854775808)
        i97 = int_and(i85, 1)
        i98 = int_add(i96, 1)
        i99 = uint_lt(i96, i73)
        guard_true(i99, descr=<Guard0xabbbc94>)
        p100 = getarrayitem_gc_r(p72, i96, descr=<ArrayP 4>)
        guard_nonnull_class(p100, ConstClass(W_PointersObject), descr=<Guard0xab945f4>)
        p102 = getfield_gc_r(p100, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_strategy 20>)
        guard_value(p102, ConstPtr(ptr103), descr=<Guard0xab94628>)
        p104 = getfield_gc_r(p100, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst__storage 16>)
        i105 = arraylen_gc(p104, descr=<ArrayP 4>)
        i106 = uint_lt(i96, i105)
        guard_true(i106, descr=<Guard0xabbbcb8>)
        p107 = getarrayitem_gc_r(p104, i96, descr=<ArrayP 4>)
        guard_nonnull_class(p107, ConstClass(W_Character), descr=<Guard0xab9465c>)
        i110 = int_add(i85, 1)
        i112 = int_sub(i89, 1)
        setfield_gc(ConstPtr(ptr113), i112, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>)
        i115 = int_le(i112, 0)
        guard_false(i115, descr=<Guard0xab94690>)
        jump(p0, p1, i2, p3, p4, p7, p8, p10, p13, i110, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, i73, p72, i112, descr=TargetToken(180086732))
        """)

    def test_named_object_access(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | o |
        o := (10@10 corner: 100@100).
        1 to: 10000 do: [:i | o extent ].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x2f64500>)
        i95 = int_le(i86, 10000)
        guard_true(i95, descr=<Guard0x2a66458>)
        p96 = force_token()
        enter_portal_frame(1, 0)
        p99 = force_token()
        enter_portal_frame(1, 0)
        leave_portal_frame(1)
        leave_portal_frame(1)
        i105 = int_add(i86, 1)
        i107 = int_sub(i90, 1)
        setfield_gc(ConstPtr(ptr108), i107, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i110 = int_le(i107, 0)
        guard_false(i110, descr=<Guard0x2f64568>)
        jump(p0, p1, i2, p4, p5, p6, p8, p11, i105, p19, p21, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p60, p62, i107, descr=TargetToken(47188672))
        """)

    def test_very_large_integer_mul(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | li block |
        li := 2 raisedTo: 128.
        1 to: 10000 do: [:i | li * i].
        """)
        if IS_64BIT:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x2d4d7b0>)
            i79 = int_le(i70, 10000)
            guard_true(i79, descr=<Guard0x2d6c0b0>)
            p81 = call_r(ConstClass(fromint), i70, descr=<Callr 8 i EF=3>)
            guard_no_exception(descr=<Guard0x2d4d818>)
            p83 = call_r(ConstClass(rbigint.mul), p55, p81, descr=<Callr 8 rr EF=4>)
            guard_no_exception(descr=<Guard0x2d4d880>)
            i84 = getfield_gc_i(p83, descr=<FieldS rpython.rlib.rbigint.rbigint.inst_sign 16 pure>)
            i86 = int_ge(i84, 0)
            guard_true(i86, descr=<Guard0x2d6c0f8>)
            i87 = getfield_gc_i(p83, descr=<FieldS rpython.rlib.rbigint.rbigint.inst_size 24 pure>)
            i89 = int_le(i87, 2)
            guard_false(i89, descr=<Guard0x2d6c140>)
            i91 = int_add(i70, 1)
            i93 = int_sub(i74, 1)
            setfield_gc(ConstPtr(ptr94), i93, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i96 = int_le(i93, 0)
            guard_false(i96, descr=<Guard0x2d4d8e8>)
            jump(p0, p1, i2, p3, p4, p7, p8, p10, p13, p15, i91, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p55, i93, descr=TargetToken(47620368))
            """)
        else:
            self.assert_matches(traces[0].loop, """
            guard_not_invalidated(descr=<Guard0x351e58c>)
            i83 = int_le(i74, 10000)
            guard_true(i83, descr=<Guard0x35627f0>)
            f85 = call_f(ConstClass(rbigint.tolonglong), p55, descr=<CallL 8 r EF=4>)
            p87 = guard_exception(ConstClass(OverflowError), descr=<Guard0x351e5c0>)
            p89 = call_r(ConstClass(fromint), i74, descr=<Callr 4 i EF=3>)
            guard_no_exception(descr=<Guard0x351e5f4>)
            p91 = call_r(ConstClass(rbigint.mul), p55, p89, descr=<Callr 4 rr EF=4>)
            guard_no_exception(descr=<Guard0x351e628>)
            i92 = getfield_gc_i(p91, descr=<FieldS rpython.rlib.rbigint.rbigint.inst_sign 12 pure>)
            i94 = int_ge(i92, 0)
            guard_true(i94, descr=<Guard0x3562814>)
            i95 = getfield_gc_i(p91, descr=<FieldS rpython.rlib.rbigint.rbigint.inst_size 16 pure>)
            i97 = int_le(i95, 2)
            guard_false(i97, descr=<Guard0x3562838>)
            i99 = int_add(i74, 1)
            i101 = int_sub(i78, 1)
            setfield_gc(ConstPtr(ptr102), i101, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 32>)
            i104 = int_le(i101, 0)
            guard_false(i104, descr=<Guard0x351e65c>)
            jump(p0, p1, i2, p3, p4, p7, p8, p10, p13, p15, i99, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, p55, i101, descr=TargetToken(55924992))
            """)


    def test_float_mul(self, spy, tmpdir):
        traces = self.run(spy, tmpdir, """
        | f block |
        f := 13.4.
        1 to: 10000 do: [:i | f * i].
        """)
        self.assert_matches(traces[0].loop, """
        guard_not_invalidated(descr=<Guard0x39d74d8>)
        i76 = int_le(i67, 10000)
        guard_true(i76, descr=<Guard0x39bbe80>)
        f77 = cast_int_to_float(i67)
        f78 = float_mul(f63, f77)
        i80 = int_add(i67, 1)
        i82 = int_sub(i71, 1)
        setfield_gc(ConstPtr(ptr83), i82, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i85 = int_le(i82, 0)
        guard_false(i85, descr=<Guard0x39d7540>)
        jump(p0, p1, i2, p3, p4, p7, p8, p10, p13, p15, i80, p23, p25, p27, p29, p31, p33, p35, p37, p39, p41, p43, f63, i82, descr=TargetToken(60603936))
        """)
