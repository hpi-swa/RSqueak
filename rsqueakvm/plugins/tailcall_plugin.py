from rsqueakvm.plugins.plugin import Plugin

from rpython.rlib import jit


class TailcallPlugin(Plugin):

    def setup(self):
        _patch_contexts()

    def patch(self):
        _patch_interpreter()

    def is_optional(self):
        return True


def _patch_contexts():
    from rsqueakvm.storage_contexts import (
        ContextPartShadow, ExtraContextAttributes)

    def is_tailcall_context(self):
        return jit.promote(self.get_extra_data()._is_tailcall_context)
    def set_is_tailcall_context(self):
        self.get_extra_data()._is_tailcall_context = True
    ContextPartShadow.is_tailcall_context = is_tailcall_context
    ContextPartShadow.set_is_tailcall_context = set_is_tailcall_context

    ExtraContextAttributes._attrs_ += ['_is_tailcall_context']
    original_init = ExtraContextAttributes.__init__
    def init_with_tailcallmarker(self):
        original_init(self)
        self._is_tailcall_context = False
    ExtraContextAttributes.__init__ = init_with_tailcallmarker


def _patch_interpreter():
    from rsqueakvm.interpreter import Interpreter
    original_stack_frame = Interpreter.stack_frame
    @jit.unroll_safe
    def stack_frame(self, s_frame, old_s_frame, may_context_switch):
        if old_s_frame.is_tailcall_context() \
           and s_frame.w_method() is old_s_frame.w_method() \
           and old_s_frame.pc() == s_frame.w_method.end_pc():
            old_s_frame.store_pc(0)
            for i in range(0, len(old_s_frame._temps_and_stack)):
                old_s_frame._temps_and_stack[i] = s_frame._temps_and_stack[i]
            old_s_frame._stack_ptr = old_s_frame.tempsize()
            import pdb; pdb.set_trace()
            old_s_frame.initialize_temps(self.space, w_arguments)
        else:
            return original_stack_frame(self, s_frame, old_s_frame, may_context_switch)
    Interpreter.stack_frame = stack_frame


plugin = TailcallPlugin()


@plugin.expose_primitive(unwrap_spec=[object])
def primitiveMarkTailcallContext(interp, s_frame, w_recv):
    s_frame.set_is_tailcall_context()
    return w_recv


@plugin.expose_primitive(unwrap_spec=[object])
def primitiveIsTailcallContext(interp, s_frame, w_recv):
    return s_frame.is_tailcall_context()

"""
'From Squeak6.0alpha of 5 December 2016 [latest update: #16859] on 21 December 2016 at 1:53:02 pm'!

!Object methodsFor: '*tailcall' stamp: 'topa 12/21/2016 13:52'!
tailrecursive
	<primitive: 'primitiveMarkTailcallContext' module: 'TailcallPlugin'>! !

"""
