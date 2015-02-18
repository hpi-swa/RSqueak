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
             setfield_gc(p202, i190, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
             setarrayitem_gc(p108, 1, p202, descr=<ArrayP 4>),
             setarrayitem_gc(p80, 0, p200, descr=<ArrayP 4>),
             setfield_gc(p68, 2, descr=<FieldU spyvm.strategy.ContextPartShadow.inst__stack_ptr 32>),
             setfield_gc(p68, 15, descr=<FieldS spyvm.strategy.ContextPartShadow.inst__pc 24>),
             setfield_gc(p68, p0, descr=<FieldP spyvm.strategy.ContextPartShadow.inst__s_sender 28>),
             setfield_gc(ConstPtr(ptr82), i89, descr=<FieldS spyvm.interpreter.Interpreter.inst_remaining_stack_depth 40>),
             setarrayitem_gc(p80, 1, p202, descr=<ArrayP 4>),
             guard_class(p200, 23083152, descr=<Guard0x2eb3350>),
             p203 = getfield_gc(p200, descr=<FieldP spyvm.model.W_AbstractObjectWithClassReference.inst_w_class 12>),
             p204 = getfield_gc(p203, descr=<FieldP spyvm.model.W_PointersObject.inst_shadow 16>),
             guard_value(p204, ConstPtr(ptr121), descr=<Guard0x2eb32d0>),
             guard_not_invalidated(descr=<Guard0x2eb3250>),
             p205 = getfield_gc(p200, descr=<FieldP spyvm.model.W_PointersObject.inst_shadow 16>),
             setarrayitem_gc(p80, 0, ConstPtr(null), descr=<ArrayP 4>),
             setfield_gc(p68, 0, descr=<FieldU spyvm.strategy.ContextPartShadow.inst__stack_ptr 32>),
             setfield_gc(ConstPtr(ptr82), i136, descr=<FieldS spyvm.interpreter.Interpreter.inst_remaining_stack_depth 40>),
             setarrayitem_gc(p80, 1, ConstPtr(null), descr=<ArrayP 4>),
             guard_class(p205, ConstClass(ListStrategy), descr=<Guard0x2eb31d0>),
             p208 = getfield_gc_pure(p205, descr=<FieldP spyvm.strategy.ListStrategy.inst_storage 16>),
             p209 = getarrayitem_gc(p208, 2, descr=<ArrayP 4>),
             p210 = getarrayitem_gc(p208, 0, descr=<ArrayP 4>),
             guard_class(p210, 23083152, descr=<Guard0x2eb3150>),
             p211 = getfield_gc(p210, descr=<FieldP spyvm.model.W_AbstractObjectWithClassReference.inst_w_class 12>),
             p212 = getfield_gc(p211, descr=<FieldP spyvm.model.W_PointersObject.inst_shadow 16>),
             guard_value(p212, ConstPtr(ptr154), descr=<Guard0x2eb30d0>),
             p213 = getfield_gc(p210, descr=<FieldP spyvm.model.W_PointersObject.inst_shadow 16>),
             guard_nonnull_class(p213, 23088412, descr=<Guard0x2eb3050>),
             p214 = getfield_gc_pure(p213, descr=<FieldP spyvm.strategy.SmallIntegerOrNilStrategy.inst_storage 16>),
             i215 = arraylen_gc(p214, descr=<ArrayS 4>),
             i216 = getfield_gc_pure(p213, descr=<FieldU spyvm.strategy.AbstractStrategy.inst_space 12>),
             guard_nonnull_class(p209, 23083336, descr=<Guard0x2ea3f90>),
             i217 = getfield_gc_pure(p209, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
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
             setfield_gc(p68, -1, descr=<FieldS spyvm.strategy.ContextPartShadow.inst__pc 24>),
             setfield_gc(p68, ConstPtr(null), descr=<FieldP spyvm.strategy.ContextPartShadow.inst__s_sender 28>),
             setfield_gc(ConstPtr(ptr82), i85, descr=<FieldS spyvm.interpreter.Interpreter.inst_remaining_stack_depth 40>),
             i224 = int_eq(i223, 2147483647),
             guard_false(i224, descr=<Guard0x2ea3c90>),
             i225 = int_add_ovf(i190, i223),
             guard_no_overflow(descr=<Guard0x2ea3c10>),
             i226 = int_sub(i193, 5),
             setfield_gc(ConstPtr(ptr82), i226, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
             i227 = int_le(i226, 0),
             guard_false(i227, descr=<Guard0x2ea3b90>),
             p228 = new_with_vtable(23083336),
             setfield_gc(p228, i219, descr=<FieldS spyvm.model.W_SmallInteger.inst_value 8>),
             setarrayitem_gc(p208, 2, p228, descr=<ArrayP 4>),
             i229 = arraylen_gc(p54, descr=<ArrayS 4>),
             i230 = arraylen_gc(p80, descr=<ArrayP 4>),
             i231 = arraylen_gc(p108, descr=<ArrayP 4>),
             jump(p0, p3, p6, i225, p14, p16, p18, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p54, i76, p68, i106, p92, p108, p80, i89, i91, i136, i85, i226, descr=TargetToken(48645456))
        """)

    @py.test.mark.skipif("'not working'")
    def test_indexOf(self, spy, tmpdir):
        traces = self.run(spy, tmpdir,
        """
        (1 to: 10000000) asOrderedCollection indexOf: 9999999.
        """)
        # First loop: asOrderedCollection, second loop: makeRoomAtLast
        self.assert_matches(traces[2].loop, """
             i144 = int_le(i137, i63),
             guard_true(i144, descr=<Guard0x3228ad0>),
             guard_not_invalidated(descr=<Guard0x3228850>),
             setfield_gc(ConstPtr(ptr85), i92, descr=<FieldS spyvm.interpreter.Interpreter.inst_remaining_stack_depth 40>),
             i145 = int_add_ovf(i137, i101),
             guard_no_overflow(descr=<Guard0x32283d0>),
             i146 = int_sub(i145, 1),
             i147 = int_gt(i146, i109),
             guard_false(i147, descr=<Guard0x32282d0>),
             i148 = int_sub(i146, 1),
             i149 = int_ge(i148, 0),
             guard_true(i149, descr=<Guard0x3319dd0>),
             i150 = int_lt(i148, i127),
             guard_true(i150, descr=<Guard0x3319cd0>),
             i151 = getarrayitem_gc(p126, i148, descr=<ArrayS 4>),
             i152 = int_eq(i151, 2147483647),
             guard_false(i152, descr=<Guard0x3217a50>),
             setfield_gc(ConstPtr(ptr85), i88, descr=<FieldS spyvm.interpreter.Interpreter.inst_remaining_stack_depth 40>),
             i153 = int_eq(i151, i134),
             guard_false(i153, descr=<Guard0x3217b10>),
             i154 = int_add_ovf(i137, 1),
             guard_no_overflow(descr=<Guard0x3217ad0>),
             i155 = int_sub(i140, 3),
             setfield_gc(ConstPtr(ptr85), i155, descr=<FieldS spyvm.interpreter.Interpreter.inst_interrupt_check_counter 24>),
             i156 = int_le(i155, 0),
             guard_false(i156, descr=<Guard0x3217a90>),
             i157 = arraylen_gc(p97, descr=<ArrayP 4>),
             jump(p0, p3, p6, p8, p10, i154, p14, p20, p22, p24, p26, p28, p30, p32, p34, p36, p38, p40, p42, p44, p46, p48, p50, p52, i63, p65, i92, i101, p99, i109, p106, p112, i127, p126, i88, i134, i155, p97, descr=TargetToken(53728496))
        """)
