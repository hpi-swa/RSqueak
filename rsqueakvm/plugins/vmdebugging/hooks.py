from rsqueakvm.objspace import ConstantFlag
from rsqueakvm.plugins.vmdebugging.model import wrap_oplist, wrap_greenkey, wrap_debug_info

from rpython.rlib.jit import JitHookInterface, Counters


jit_iface_recursion = ConstantFlag()
def make_hook(args, func):
    import inspect, re
    src = "\n".join([
        re.sub("^\\s+", " " * 12, line) for line in inspect.getsource(func).split("\n")[1:]
    ])
    code = [
        "def f(%s):" % (args),
        "    from rsqueakvm import constants",
        "    from rsqueakvm.interpreter import jit_driver_name",
        "    from rsqueakvm.model.variable import W_BytesObject",
        "    if jitdriver.name != jit_driver_name: return",
        "    space = self.space",
        "    if jit_iface_recursion.is_set(): return",
        "    interp = space.interp.get()",
        "    w_jithook = space.w_jit_hook_selector",
        "    w_rcvr = space.w_jit_hook_receiver",
        "    if w_jithook and isinstance(w_jithook, W_BytesObject) and w_rcvr:",
        "        w_method = w_rcvr.class_shadow(space).lookup(w_jithook)",
        "        if w_method is None: return",
        "        jit_iface_recursion.activate()",
        "        try:",
        "            args_w = func(%s)" % args,
        "            interp.perform_headless(w_rcvr, w_jithook, [space.wrap_list(args_w)])",
        "        finally:",
        "            jit_iface_recursion.deactivate()"
    ]
    d = {
        "jit_iface_recursion": jit_iface_recursion,
        "func": func,
    }
    exec compile("\n".join(code), __file__, 'exec') in d
    return d["f"]


class JitIface(JitHookInterface):
    def prepare_abort(self, reason, jitdriver, greenkey, greenkey_repr, logops, operations):
        space = self.space
        return [
            space.wrap_string('abort'),
            wrap_greenkey(space, jitdriver, greenkey, greenkey_repr),
            space.wrap_string(Counters.counter_names[reason]),
            wrap_oplist(space, logops, operations)]

    on_abort = make_hook(
        "self, reason, jitdriver, greenkey, greenkey_repr, logops, operations",
        prepare_abort
    )

    def prepare_trace_too_long(self, jitdriver, greenkey, greenkey_repr):
        space = self.space
        return [
            space.wrap_string('trace_too_long'),
            wrap_greenkey(space, jitdriver, greenkey, greenkey_repr)]

    on_trace_too_long = make_hook(
        "self, jitdriver, greenkey, greenkey_repr",
        prepare_trace_too_long
    )

    def prepare_compile_hook(self, jitdriver, debug_info, is_bridge):
        space = self.space
        return [
            space.wrap_string('compile_loop' if not is_bridge else 'compile_bridge'),
            wrap_debug_info(space, debug_info, is_bridge=is_bridge)]

    wrapped_compiled_hook = make_hook(
        "self, jitdriver, debug_info, is_bridge",
        prepare_compile_hook
    )

    def _compile_hook(self, debug_info, is_bridge=False):
        jitdriver = debug_info.get_jitdriver()
        self.wrapped_compiled_hook(jitdriver, debug_info, is_bridge)

    def after_compile(self, debug_info): self._compile_hook(debug_info, is_bridge=False)
    def after_compile_bridge(self, debug_info): self._compile_hook(debug_info, is_bridge=True)
    def before_compile(self, debug_info): pass
    def before_compile_bridge(self, debug_info): pass


jitiface = JitIface()
