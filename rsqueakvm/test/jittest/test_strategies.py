import py

from .base import BaseJITTest

class TestBasic(BaseJITTest):

     # TODO: there shouldnt be allocations in this
     # The cond_call operations should also not show up...
    @py.test.mark.skipif("'not working'")
    def test_range_asOrderedCollection(self, spy, tmpdir):
        traces = self.run(spy, tmpdir,
        """
        (1 to: 10000) asOrderedCollection.
        """)
        self.assert_matches(traces[0].loop, """
             i197 = getarrayitem_gc(p54, 1, descr=<ArrayS 4>),
             i198 = int_eq(i197, 2147483647),
             guard_false(i198, descr=<Guard0x2eb3450>),
             i199 = int_ge(i197, i190),
             guard_true(i199, descr=<Guard0x2eb33d0>),
             cond_call(i76, 21753520, p68, descr=<Callv 0 r EF=2 OS=121>),
             cond_call(i106, 21753520, p92, descr=<Callv 0 r EF=2 OS=121>),
             cond_call(i106, 21753520, p92, descr=<Callv 0 r EF=2 OS=121>),
             p200 = getarrayitem_gc(p108, 0, descr=<ArrayP 4>),
             cond_call(i106, 21753520, p92, descr=<Callv 0 r EF=2 OS=121>),
             p202 = new_with_vtable(23083336),
             setfield_gc(p202, i190, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8>),
             setarrayitem_gc(p108, 1, p202, descr=<ArrayP 4>),
             setarrayitem_gc(p80, 0, p200, descr=<ArrayP 4>),
             setfield_gc(p68, 2, descr=<FieldU rsqueakvm.strategy.ContextPartShadow.inst__stack_ptr 32>),
             setfield_gc(p68, 15, descr=<FieldS rsqueakvm.strategy.ContextPartShadow.inst__pc 24>),
             setfield_gc(p68, p0, descr=<FieldP rsqueakvm.strategy.ContextPartShadow.inst__s_sender 28>),
             setfield_gc(ConstPtr(ptr82), i89, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_remaining_stack_depth 40>),
             setarrayitem_gc(p80, 1, p202, descr=<ArrayP 4>),
             guard_class(p200, 23083152, descr=<Guard0x2eb3350>),
             p203 = getfield_gc(p200, descr=<FieldP rsqueakvm.model.base.W_AbstractObjectWithClassReference.inst_w_class 12>),
             p204 = getfield_gc(p203, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_shadow 16>),
             guard_value(p204, ConstPtr(ptr121), descr=<Guard0x2eb32d0>),
             guard_not_invalidated(descr=<Guard0x2eb3250>),
             p205 = getfield_gc(p200, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_shadow 16>),
             setarrayitem_gc(p80, 0, ConstPtr(null), descr=<ArrayP 4>),
             setfield_gc(p68, 0, descr=<FieldU rsqueakvm.strategy.ContextPartShadow.inst__stack_ptr 32>),
             setfield_gc(ConstPtr(ptr82), i136, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_remaining_stack_depth 40>),
             setarrayitem_gc(p80, 1, ConstPtr(null), descr=<ArrayP 4>),
             guard_class(p205, ConstClass(ListStrategy), descr=<Guard0x2eb31d0>),
             p208 = getfield_gc_pure(p205, descr=<FieldP rsqueakvm.strategy.ListStrategy.inst_storage 16>),
             p209 = getarrayitem_gc(p208, 2, descr=<ArrayP 4>),
             p210 = getarrayitem_gc(p208, 0, descr=<ArrayP 4>),
             guard_class(p210, 23083152, descr=<Guard0x2eb3150>),
             p211 = getfield_gc(p210, descr=<FieldP rsqueakvm.model.base.W_AbstractObjectWithClassReference.inst_w_class 12>),
             p212 = getfield_gc(p211, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_shadow 16>),
             guard_value(p212, ConstPtr(ptr154), descr=<Guard0x2eb30d0>),
             p213 = getfield_gc(p210, descr=<FieldP rsqueakvm.model.pointers.W_PointersObject.inst_shadow 16>),
             guard_nonnull_class(p213, 23088412, descr=<Guard0x2eb3050>),
             p214 = getfield_gc_pure(p213, descr=<FieldP rsqueakvm.strategy.SmallIntegerOrNilStrategy.inst_storage 16>),
             i215 = arraylen_gc(p214, descr=<ArrayS 4>),
             i216 = getfield_gc_pure(p213, descr=<FieldU rsqueakvm.strategy.AbstractStrategy.inst_space 12>),
             guard_nonnull_class(p209, 23083336, descr=<Guard0x2ea3f90>),
             i217 = getfield_gc_pure(p209, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8>),
             i218 = int_eq(i217, i215),
             guard_false(i218, descr=<Guard0x2ea3f10>),
             i219 = int_add_ovf(i217, 1),
             guard_no_overflow(descr=<Guard0x2ea3e90>),
             i220 = int_ge(i217, 0),
             guard_true(i220, descr=<Guard0x2ea3e10>),
             i221 = int_lt(i217, i215),
             guard_true(i221, descr=<Guard0x2ea3d90>),
             i222 = int_eq(i190, 2147483647),
             guard_false(i222, descr=<Guard0x2ea3d10>),
             setarrayitem_gc(p214, i217, i190, descr=<ArrayS 4>),
             i223 = getarrayitem_gc(p54, 2, descr=<ArrayS 4>),
             setfield_gc(p68, -1, descr=<FieldS rsqueakvm.strategy.ContextPartShadow.inst__pc 24>),
             setfield_gc(p68, ConstPtr(null), descr=<FieldP rsqueakvm.strategy.ContextPartShadow.inst__s_sender 28>),
             setfield_gc(ConstPtr(ptr82), i85, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_remaining_stack_depth 40>),
             i224 = int_eq(i223, 2147483647),
             guard_false(i224, descr=<Guard0x2ea3c90>),
             i225 = int_add_ovf(i190, i223),
             guard_no_overflow(descr=<Guard0x2ea3c10>),
             i226 = int_sub(i193, 5),
             setfield_gc(ConstPtr(ptr82), i226, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
             i227 = int_le(i226, 0),
             guard_false(i227, descr=<Guard0x2ea3b90>),
             p228 = new_with_vtable(23083336),
             setfield_gc(p228, i219, descr=<FieldS rsqueakvm.model.numeric.W_SmallInteger.inst_value 8>),
             setarrayitem_gc(p208, 2, p228, descr=<ArrayP 4>),
             i229 = arraylen_gc(p54, descr=<ArrayS 4>),
             i230 = arraylen_gc(p80, descr=<ArrayP 4>),
             i231 = arraylen_gc(p108, descr=<ArrayP 4>),
             jump(p0, p3, p6, i225, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p54, i76, p68, i106, p92, p108, p80, i89, i91, i136, i85, i226, descr=TargetToken(48645456))
        """)

    def test_indexOf(self, spy, tmpdir):
        traces = self.run(spy, tmpdir,
        """
        (1 to: 10000000) asOrderedCollection indexOf: 9999999.
        """)
        # First loop: asOrderedCollection, second loop: makeRoomAtLast
        self.assert_matches(traces[3].loop, """
        guard_not_invalidated(descr=<Guard0x385d640>)
        i74 = int_le(i66, i36)
        guard_true(i74, descr=<Guard0x2fcfb20>)
        p75 = force_token()
        enter_portal_frame(1, 100)
        i78 = int_add_ovf(i66, i43)
        guard_no_overflow(descr=<Guard0x2fcfa48>)
        i80 = int_sub(i78, 1)
        i81 = int_gt(i80, i47)
        guard_false(i81, descr=<Guard0x2fcfc88>)
        i83 = int_sub(i80, 1)
        i84 = uint_lt(i83, i56)
        guard_true(i84, descr=<Guard0x2fcfcd0>)
        i85 = getarrayitem_gc_i(p55, i83, descr=<ArrayS 8>)
        i87 = int_eq(i85, 9223372036854775807)
        guard_false(i87, descr=<Guard0x2fcfd18>)
        leave_portal_frame(1)
        i89 = int_eq(i85, i63)
        guard_false(i89, descr=<Guard0x2fcfd60>)
        i91 = int_add_ovf(i66, 1)
        guard_no_overflow(descr=<Guard0x2fcfda8>)
        i93 = int_sub(i70, 1)
        setfield_gc(ConstPtr(ptr94), i93, descr=<FieldS rsqueakvm.interpreter.Interpreter.inst_interrupt_check_counter 24>)
        i96 = int_le(i93, 0)
        guard_false(i96, descr=<Guard0x385d6a0>)
        jump(p0, p1, p3, p4, p5, p10, p12, p14, i91, p18, p24, i36, i43, i47, p49, i56, p55, i63, i93, descr=TargetToken(54058928))
        """)
