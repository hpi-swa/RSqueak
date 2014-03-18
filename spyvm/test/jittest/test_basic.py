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
         i33 = getfield_gc(p0, descr=<FieldU spyvm.shadow.AbstractShadow.inst_space 12>),
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
            guard_not_invalidated(descr=<Guard0x3bc1e10>)
            p540 = getarrayitem_gc(p152, 30, descr=<ArrayP 4>)
            guard_nonnull_class(p540, 19336136, descr=<Guard0x3bc18d0>)
            i541 = getfield_gc_pure(p540, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            i543 = int_le(2, i541)
            guard_false(i543, descr=<Guard0x3bc1850>)
            p544 = getarrayitem_gc(p152, 34, descr=<ArrayP 4>)
            p545 = getarrayitem_gc(p152, 36, descr=<ArrayP 4>)
            guard_nonnull_class(p544, 19336136, descr=<Guard0x3bc11d0>)
            i546 = getfield_gc_pure(p544, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            guard_nonnull_class(p545, 19336136, descr=<Guard0x3bc1150>)
            i547 = getfield_gc_pure(p545, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            i548 = int_add_ovf(i546, i547)
            guard_no_overflow(descr=<Guard0x3bc10d0>)
            p549 = getarrayitem_gc(p152, 35, descr=<ArrayP 4>)
            p550 = getarrayitem_gc(p152, 37, descr=<ArrayP 4>)
            guard_nonnull_class(p549, 19336136, descr=<Guard0x3bbe990>)
            i551 = getfield_gc_pure(p549, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            guard_nonnull_class(p550, 19336136, descr=<Guard0x3bbe910>)
            i552 = getfield_gc_pure(p550, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            i553 = int_add_ovf(i551, i552)
            guard_no_overflow(descr=<Guard0x3bbe890>)
            i554 = int_add_ovf(i181, 1)
            guard_no_overflow(descr=<Guard0x3bbe390>)
            i555 = int_sub(i527, 2)
            setfield_gc(ConstPtr(ptr182), i555, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i556 = int_le(i555, 0)
            guard_false(i556, descr=<Guard0x3bbe110>)
            p557 = getarrayitem_gc(p152, 16, descr=<ArrayP 4>)
            guard_nonnull_class(p557, 19336136, descr=<Guard0x3bbba90>)
            i558 = getfield_gc_pure(p557, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            i559 = int_le(i554, i558)
            guard_true(i559, descr=<Guard0x3bbba10>)
            p560 = getarrayitem_gc(p152, 2, descr=<ArrayP 4>)
            guard_class(p560, 19336008, descr=<Guard0x3bbb590>)
            p561 = getfield_gc(p560, descr=<FieldP spyvm.model.W_AbstractObjectWithClassReference.inst_s_class 12>)
            guard_value(p561, ConstPtr(ptr198), descr=<Guard0x3bbb550>)
            p562 = getarrayitem_gc(p152, 25, descr=<ArrayP 4>)
            p563 = getarrayitem_gc(p152, 20, descr=<ArrayP 4>)
            guard_class(p562, 19352312, descr=<Guard0x3bb8a10>)
            p564 = getfield_gc(p562, descr=<FieldP spyvm.model.W_AbstractObjectWithClassReference.inst_s_class 12>)
            guard_value(p564, ConstPtr(ptr206), descr=<Guard0x3bb89d0>)
            i565 = getfield_gc_pure(p562, descr=<FieldS spyvm.model.W_WordsObject.inst__size 20>)
            guard_nonnull_class(p563, 19336136, descr=<Guard0x3bb86d0>)
            i566 = getfield_gc_pure(p563, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            i567 = int_is_zero(i565)
            guard_false(i567, descr=<Guard0x3bb8650>)
            i568 = int_mod(i566, i565)
            i569 = int_lt(i565, 0)
            guard_false(i569, descr=<Guard0x3bb8610>)
            i570 = int_rshift(i568, 31)
            i571 = int_and(i565, i570)
            i572 = int_add(i568, i571)
            i573 = int_add_ovf(1, i572)
            guard_no_overflow(descr=<Guard0x3bb8510>)
            i574 = int_ge(i572, 0)
            guard_true(i574, descr=<Guard0x3bb8150>)
            i575 = int_lt(i572, i565)
            guard_true(i575, descr=<Guard0x3bb8110>)
            p576 = getfield_gc(p562, descr=<FieldP spyvm.model.W_WordsObject.inst_words 28>)
            guard_nonnull(p576, descr=<Guard0x3bb80d0>)
            i577 = getarrayitem_gc(p576, i572, descr=<ArrayU 4>)
            i578 = uint_lt(i577, 0)
            guard_false(i578, descr=<Guard0x3bb8090>)
            i579 = uint_lt(i577, 2147483647)
            guard_true(i579, descr=<Guard0x3bb8050>)
            p580 = getarrayitem_gc(p152, 32, descr=<ArrayP 4>)
            guard_nonnull_class(p580, 19336136, descr=<Guard0x3bb69d0>)
            i581 = getfield_gc_pure(p580, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            i582 = int_add_ovf(i566, i581)
            guard_no_overflow(descr=<Guard0x3bb6950>)
            p583 = getarrayitem_gc(p152, 33, descr=<ArrayP 4>)
            i584 = instance_ptr_eq(p583, ConstPtr(ptr236))
            guard_true(i584, descr=<Guard0x3bb6090>)
            p585 = getarrayitem_gc(p152, 27, descr=<ArrayP 4>)
            i586 = int_le(1, i541)
            guard_true(i586, descr=<Guard0x3bb3410>)
            p587 = getarrayitem_gc(p152, 1, descr=<ArrayP 4>)
            guard_class(p587, 19336008, descr=<Guard0x3bb1f50>)
            p588 = getfield_gc(p587, descr=<FieldP spyvm.model.W_AbstractObjectWithClassReference.inst_s_class 12>)
            guard_value(p588, ConstPtr(ptr246), descr=<Guard0x3bb1f10>)
            i589 = int_ge(i577, 0)
            guard_true(i589, descr=<Guard0x3bb1910>)
            i590 = int_and(i577, i577)
            i591 = uint_lt(i590, 2147483647)
            guard_true(i591, descr=<Guard0x3bb1890>)
            p592 = getarrayitem_gc(p152, 21, descr=<ArrayP 4>)
            i593 = int_add_ovf(i553, 1)
            guard_no_overflow(descr=<Guard0x3bb1250>)
            guard_class(p592, 19375152, descr=<Guard0x3bb10d0>)
            p594 = getfield_gc(p592, descr=<FieldP spyvm.model.W_AbstractObjectWithClassReference.inst_s_class 12>)
            guard_value(p594, ConstPtr(ptr258), descr=<Guard0x3bb1090>)
            i595 = int_ge(i553, 0)
            guard_true(i595, descr=<Guard0x3bafe50>)
            i596 = getfield_gc_pure(p592, descr=<FieldS spyvm.model.W_DisplayBitmap.inst__realsize 28>)
            i597 = int_lt(i553, i596)
            guard_true(i597, descr=<Guard0x3bafe10>)
            i598 = getfield_gc(p592, descr=<FieldU spyvm.model.W_DisplayBitmap.inst__real_depth_buffer 24>)
            i599 = getarrayitem_raw(i598, i553, descr=<ArrayU 4>)
            i600 = uint_lt(i599, 0)
            guard_false(i600, descr=<Guard0x3bafdd0>)
            i601 = uint_lt(i599, 2147483647)
            guard_true(i601, descr=<Guard0x3bafd90>)
            p602 = getarrayitem_gc(p152, 3, descr=<ArrayP 4>)
            setfield_gc(ConstPtr(ptr182), i276, descr=<FieldS spyvm.interpreter.Interpreter.inst_remaining_stack_depth 40>)
            guard_nonnull_class(p602, 19336136, descr=<Guard0x3baf310>)
            i603 = getfield_gc_pure(p602, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            i604 = int_lt(i603, 16)
            guard_true(i604, descr=<Guard0x3baf290>)
            i605 = int_eq(i603, 0)
            guard_false(i605, descr=<Guard0x3bacd50>)
            i606 = int_eq(i603, 1)
            guard_false(i606, descr=<Guard0x3bac910>)
            i607 = int_eq(i603, 2)
            guard_false(i607, descr=<Guard0x3bac4d0>)
            i608 = int_eq(i603, 3)
            guard_true(i608, descr=<Guard0x3ba8f90>)
            setfield_gc(ConstPtr(ptr182), i272, descr=<FieldS spyvm.interpreter.Interpreter.inst_remaining_stack_depth 40>)
            guard_nonnull_class(p585, 19351904, descr=<Guard0x3ba8110>)
            i609 = getfield_gc(p585, descr=<FieldS spyvm.model.W_LargePositiveInteger1Word.inst_value 16>)
            i610 = int_and(i609, i590)
            i611 = uint_lt(i610, 2147483647)
            guard_true(i611, descr=<Guard0x3ba8050>)
            i612 = int_xor(i609, i297)
            setfield_gc(ConstPtr(ptr182), i276, descr=<FieldS spyvm.interpreter.Interpreter.inst_remaining_stack_depth 40>)
            i613 = uint_lt(i612, 2147483647)
            guard_true(i613, descr=<Guard0x3ba6390>)
            i614 = getarrayitem_raw(i598, i553, descr=<ArrayU 4>)
            setfield_gc(ConstPtr(ptr182), i272, descr=<FieldS spyvm.interpreter.Interpreter.inst_remaining_stack_depth 40>)
            i615 = uint_lt(i614, 0)
            guard_false(i615, descr=<Guard0x3ba4550>)
            i616 = uint_lt(i614, 2147483647)
            guard_true(i616, descr=<Guard0x3ba4510>)
            i617 = int_ge(i612, 0)
            guard_true(i617, descr=<Guard0x3ba4410>)
            i618 = int_ge(i614, 0)
            guard_true(i618, descr=<Guard0x3ba43d0>)
            i619 = int_and(i612, i614)
            i620 = uint_lt(i619, 2147483647)
            guard_true(i620, descr=<Guard0x3ba4390>)
            i621 = int_ge(i610, 0)
            guard_true(i621, descr=<Guard0x3ba4290>)
            i622 = int_or(i610, i619)
            i623 = uint_lt(i622, 2147483647)
            guard_true(i623, descr=<Guard0x3ba4210>)
            setarrayitem_raw(i598, i553, i622, descr=<ArrayU 4>)
            i624 = getfield_gc_pure(p592, descr=<FieldS spyvm.model.W_DisplayBitmap.inst__depth 20>)
            i625 = int_floordiv(8, i624)
            i626 = int_mul(i625, i624)
            i627 = int_lt(i624, 0)
            guard_false(i627, descr=<Guard0x3b2fd90>)
            i628 = int_sub(8, i626)
            i629 = int_rshift(i628, 31)
            i630 = int_add(i625, i629)
            i631 = int_mul(i553, i630)
            i632 = int_sub(32, i624)
            i633 = int_ge(0, i630)
            guard_false(i633, descr=<Guard0x3b20110>)
            i634 = int_ge(i631, i596)
            guard_false(i634, descr=<Guard0x3b200d0>)
            i635 = uint_rshift(i622, i632)
            i636 = int_lshift(i622, i624)
            i637 = uint_rshift(i636, i632)
            i638 = int_lshift(i637, 8)
            i639 = int_or(i635, i638)
            i640 = int_lshift(i636, i624)
            i641 = uint_rshift(i640, i632)
            i642 = int_lshift(i641, 16)
            i643 = int_or(i639, i642)
            i644 = int_lshift(i640, i624)
            i645 = uint_rshift(i644, i632)
            i646 = int_lshift(i645, 24)
            i647 = int_or(i643, i646)
            i648 = int_lshift(i644, i624)
            p649 = getfield_gc_pure(p592, descr=<FieldP spyvm.model.W_DisplayBitmap.inst_display 32>)
            i650 = getfield_gc(p649, descr=<FieldU spyvm.display.SDLDisplay.inst_pixelbuffer 36>)
            guard_value(i650, 59506760, descr=<Guard0x3b20050>)
            setarrayitem_raw(59506760, i631, i647, descr=<ArrayU 4>)
            i651 = int_add(i631, 1)
            i652 = int_ge(1, i630)
            guard_false(i652, descr=<Guard0x3b1ff90>)
            i653 = int_ge(i651, i596)
            guard_false(i653, descr=<Guard0x3b1ff10>)
            i654 = uint_rshift(i648, i632)
            i655 = int_lshift(i648, i624)
            i656 = uint_rshift(i655, i632)
            i657 = int_lshift(i656, 8)
            i658 = int_or(i654, i657)
            i659 = int_lshift(i655, i624)
            i660 = uint_rshift(i659, i632)
            i661 = int_lshift(i660, 16)
            i662 = int_or(i658, i661)
            i663 = int_lshift(i659, i624)
            i664 = uint_rshift(i663, i632)
            i665 = int_lshift(i664, 24)
            i666 = int_or(i662, i665)
            i667 = int_lshift(i663, i624)
            setarrayitem_raw(59506760, i651, i666, descr=<ArrayU 4>)
            i668 = int_add(i651, 1)
            i669 = int_ge(2, i630)
            guard_false(i669, descr=<Guard0x3b1fe90>)
            i670 = int_ge(i668, i596)
            guard_false(i670, descr=<Guard0x3b1fe50>)
            i671 = uint_rshift(i667, i632)
            i672 = int_lshift(i667, i624)
            i673 = uint_rshift(i672, i632)
            i674 = int_lshift(i673, 8)
            i675 = int_or(i671, i674)
            i676 = int_lshift(i672, i624)
            i677 = uint_rshift(i676, i632)
            i678 = int_lshift(i677, 16)
            i679 = int_or(i675, i678)
            i680 = int_lshift(i676, i624)
            i681 = uint_rshift(i680, i632)
            i682 = int_lshift(i681, 24)
            i683 = int_or(i679, i682)
            i684 = int_lshift(i680, i624)
            setarrayitem_raw(59506760, i668, i683, descr=<ArrayU 4>)
            i685 = int_add(i668, 1)
            i686 = int_ge(3, i630)
            guard_false(i686, descr=<Guard0x3b1fdd0>)
            i687 = int_ge(i685, i596)
            guard_false(i687, descr=<Guard0x3b1fd90>)
            i688 = uint_rshift(i684, i632)
            i689 = int_lshift(i684, i624)
            i690 = uint_rshift(i689, i632)
            i691 = int_lshift(i690, 8)
            i692 = int_or(i688, i691)
            i693 = int_lshift(i689, i624)
            i694 = uint_rshift(i693, i632)
            i695 = int_lshift(i694, 16)
            i696 = int_or(i692, i695)
            i697 = int_lshift(i693, i624)
            i698 = uint_rshift(i697, i632)
            i699 = int_lshift(i698, 24)
            i700 = int_or(i696, i699)
            i701 = int_lshift(i697, i624)
            setarrayitem_raw(59506760, i685, i700, descr=<ArrayU 4>)
            i702 = int_add(i685, 1)
            i703 = int_ge(4, i630)
            guard_false(i703, descr=<Guard0x3b1fd10>)
            i704 = int_ge(i702, i596)
            guard_false(i704, descr=<Guard0x3b1fcd0>)
            i705 = uint_rshift(i701, i632)
            i706 = int_lshift(i701, i624)
            i707 = uint_rshift(i706, i632)
            i708 = int_lshift(i707, 8)
            i709 = int_or(i705, i708)
            i710 = int_lshift(i706, i624)
            i711 = uint_rshift(i710, i632)
            i712 = int_lshift(i711, 16)
            i713 = int_or(i709, i712)
            i714 = int_lshift(i710, i624)
            i715 = uint_rshift(i714, i632)
            i716 = int_lshift(i715, 24)
            i717 = int_or(i713, i716)
            i718 = int_lshift(i714, i624)
            setarrayitem_raw(59506760, i702, i717, descr=<ArrayU 4>)
            i719 = int_add(i702, 1)
            i720 = int_ge(5, i630)
            guard_false(i720, descr=<Guard0x3b1fc50>)
            i721 = int_ge(i719, i596)
            guard_false(i721, descr=<Guard0x3b1fc10>)
            i722 = uint_rshift(i718, i632)
            i723 = int_lshift(i718, i624)
            i724 = uint_rshift(i723, i632)
            i725 = int_lshift(i724, 8)
            i726 = int_or(i722, i725)
            i727 = int_lshift(i723, i624)
            i728 = uint_rshift(i727, i632)
            i729 = int_lshift(i728, 16)
            i730 = int_or(i726, i729)
            i731 = int_lshift(i727, i624)
            i732 = uint_rshift(i731, i632)
            i733 = int_lshift(i732, 24)
            i734 = int_or(i730, i733)
            i735 = int_lshift(i731, i624)
            setarrayitem_raw(59506760, i719, i734, descr=<ArrayU 4>)
            i736 = int_add(i719, 1)
            i737 = int_ge(6, i630)
            guard_false(i737, descr=<Guard0x3b1fb90>)
            i738 = int_ge(i736, i596)
            guard_false(i738, descr=<Guard0x3bc1f10>)
            i739 = uint_rshift(i735, i632)
            i740 = int_lshift(i735, i624)
            i741 = uint_rshift(i740, i632)
            i742 = int_lshift(i741, 8)
            i743 = int_or(i739, i742)
            i744 = int_lshift(i740, i624)
            i745 = uint_rshift(i744, i632)
            i746 = int_lshift(i745, 16)
            i747 = int_or(i743, i746)
            i748 = int_lshift(i744, i624)
            i749 = uint_rshift(i748, i632)
            i750 = int_lshift(i749, 24)
            i751 = int_or(i747, i750)
            i752 = int_lshift(i748, i624)
            setarrayitem_raw(59506760, i736, i751, descr=<ArrayU 4>)
            i753 = int_add(i736, 1)
            i754 = int_ge(7, i630)
            guard_false(i754, descr=<Guard0x3b1fb10>)
            i755 = int_ge(i753, i596)
            guard_false(i755, descr=<Guard0x3b1fad0>)
            i756 = uint_rshift(i752, i632)
            i757 = int_lshift(i752, i624)
            i758 = uint_rshift(i757, i632)
            i759 = int_lshift(i758, 8)
            i760 = int_or(i756, i759)
            i761 = int_lshift(i757, i624)
            i762 = uint_rshift(i761, i632)
            i763 = int_lshift(i762, 16)
            i764 = int_or(i760, i763)
            i765 = int_lshift(i761, i624)
            i766 = uint_rshift(i765, i632)
            i767 = int_lshift(i766, 24)
            i768 = int_or(i764, i767)
            i769 = int_lshift(i765, i624)
            setarrayitem_raw(59506760, i753, i768, descr=<ArrayU 4>)
            i770 = int_add(i753, 1)
            i771 = int_ge(8, i630)
            guard_true(i771, descr=<Guard0x3b1fa50>)
            p772 = getarrayitem_gc(p152, 31, descr=<ArrayP 4>)
            guard_nonnull_class(p772, 19336136, descr=<Guard0x3b1f3d0>)
            i773 = getfield_gc_pure(p772, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            i774 = int_add_ovf(i548, i773)
            guard_no_overflow(descr=<Guard0x3b1f350>)
            i775 = int_add_ovf(i553, i773)
            guard_no_overflow(descr=<Guard0x3b04ad0>)
            p776 = getarrayitem_gc(ConstPtr(ptr523), 1, descr=<ArrayP 4>)
            i777 = int_sub(i555, 24)
            setfield_gc(ConstPtr(ptr182), i777, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
            i778 = int_le(i777, 0)
            guard_false(i778, descr=<Guard0x33b0310>)
            p779 = new_with_vtable(19336136)
            setfield_gc(p779, i774, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            setarrayitem_gc(p152, 34, p779, descr=<ArrayP 4>)
            p780 = new_with_vtable(19336136)
            setfield_gc(p780, i775, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            setarrayitem_gc(p152, 35, p780, descr=<ArrayP 4>)
            p781 = new_with_vtable(19336136)
            setfield_gc(p781, i582, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>)
            setarrayitem_gc(p152, 20, p781, descr=<ArrayP 4>)
            i782 = arraylen_gc(p152, descr=<ArrayP 4>)
            jump(p0, p3, p8, i577, p776, i590, p18, i554, p38, p40, p42, p44, p46, p48, p50, p52, p54, p56, p58, p60, p62, p64, p66, p68, p70, p72, p74, p76, p78, p80, p82, p84, p86, p88, p90, p92, p94, p96, p98, p100, p102, p104, p106, p108, p110, p112, p114, p116, p118, p120, p122, p124, p126, p128, p130, p132, p134, i139, p152, i777, p141, i276, i272, i297, descr=TargetToken(61930080))
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
