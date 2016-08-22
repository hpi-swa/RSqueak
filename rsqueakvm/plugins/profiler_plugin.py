from rsqueakvm.util import system
if "profiler_plugin" not in system.optional_plugins:
    raise LookupError

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.plugins.plugin import Plugin, PluginPatchScripts

from rpython.rlib import rvmprof


ProfilerPlugin = Plugin()

# ____________________________________________________________

def patch_interpreter():
    from rsqueakvm.interpreter import Interpreter
    def _get_code(interp, s_frame, s_sender, may_context_switch=True):
        return s_frame.w_method()
    _decorator = rvmprof.vmprof_execute_code("rsqueak", _get_code)
    _my_stack_frame = _decorator(Interpreter.stack_frame)
    Interpreter.stack_frame = _my_stack_frame
    print "Interpreter was patched for vmprof"
# XXX: We cannot patch the interpreter here, so we use this hacked
PluginPatchScripts.append(patch_interpreter)

def _safe(s):
    if len(s) > 210:
        s = s[:207] + '...'
    return s.replace(':', ';')

def _get_full_name(w_cm):
    # must not be longer than 255 chars
    return "py:%s:0:img" % _safe(w_cm.safe_identifier_string())

rvmprof.register_code_object_class(W_CompiledMethod, _get_full_name)

def patch_compiled_method():
    def _my_post_init(self):
        rvmprof.register_code(self, _get_full_name)
    W_CompiledMethod.post_init = _my_post_init
patch_compiled_method()

# ____________________________________________________________

@ProfilerPlugin.expose_primitive(unwrap_spec=[object, int, float])
def enable(interp, s_frame, w_rcvr, fileno, period):
    try:
        rvmprof.enable(fileno, period)
    except rvmprof.VMProfError as e:
        print e.msg
        raise PrimitiveFailedError

@ProfilerPlugin.expose_primitive()
def disable(interp, s_frame, argcount):
    try:
        rvmprof.disable()
    except rvmprof.VMProfError as e:
        print e.msg
        raise PrimitiveFailedError
