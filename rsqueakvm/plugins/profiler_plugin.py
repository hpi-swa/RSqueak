from rsqueakvm.util import system
if "profiler_plugin" not in system.optional_plugins:
    raise LookupError

from rpython.rlib import rvmprof
from rpython.rlib.rjitlog import rjitlog

from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.model.compiled_methods import W_CompiledMethod
from rsqueakvm.plugins.plugin import Plugin, PluginPatchScripts
from rsqueakvm.primitives import expose_primitive, expose_also_as, pos_32bit_int
from rsqueakvm.primitives.constants import (VM_CLEAR_PROFILE, VM_DUMP_PROFILE,
                                            VM_START_PROFILING, VM_STOP_PROFILING)


ProfilerPlugin = Plugin()

# ____________________________________________________________

def patch_interpreter():
    from rpython.rlib import rvmprof
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
    if len(s) > 200:
        s = s[:197] + '...'
    return s.replace(':', ';')

def _get_full_name(w_cm):
    # must not be longer than 255 chars
    return "st:%s:0:/img" % _safe(w_cm.safe_identifier_string())

rvmprof.register_code_object_class(W_CompiledMethod, _get_full_name)

def patch_compiled_method():
    def _my_post_init(self):
        from rpython.rlib import rvmprof
        rvmprof.register_code(self, _get_full_name)
    W_CompiledMethod.post_init = _my_post_init
patch_compiled_method()

# ____________________________________________________________

@ProfilerPlugin.expose_primitive(unwrap_spec=[object, int, float])
def enableProfiler(interp, s_frame, w_rcvr, fileno, period):
    try:
        rvmprof.enable(fileno, period)
    except rvmprof.VMProfError as e:
        print e.msg
        raise PrimitiveFailedError
    return w_rcvr

@ProfilerPlugin.expose_primitive(unwrap_spec=[object])
def disableProfiler(interp, s_frame, w_rcvr):
    try:
        rvmprof.disable()
    except rvmprof.VMProfError as e:
        print e.msg
        raise PrimitiveFailedError
    return w_rcvr

@ProfilerPlugin.expose_primitive(unwrap_spec=[object, int])
def enableJitlog(interp, s_frame, w_rcvr, fileno):
    try:
        rjitlog.enable_jitlog(fileno)
    except rjitlog.JitlogError as e:
        print e.msg
        raise PrimitiveFailedError
    return w_rcvr

@ProfilerPlugin.expose_primitive(unwrap_spec=[object])
def disableJitlog(interp, s_frame, w_rcvr):
    rjitlog.disable_jitlog()
    return w_rcvr


class LogFile(object):
    _attrs_ = ["fd"]
    def __init__(self): self.fd = -1
    def set(self, v): self.fd = v
    def get(self): return self.fd
jitlogfile = LogFile()
vmproflogfile = LogFile()


@expose_also_as(VM_DUMP_PROFILE)
@expose_also_as(VM_CLEAR_PROFILE)
@expose_primitive(VM_STOP_PROFILING, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if vmproflogfile.get() > 0:
        rjitlog.disable_jitlog()
        try:
            rvmprof.disable()
        except rvmprof.VMProfError as e:
            print e.msg
            raise PrimitiveFailedError
    return w_rcvr

DEFAULT_PERIOD = 0.001
@expose_primitive(VM_START_PROFILING, unwrap_spec=[object])
def func(interp, s_frame, w_rcvr):
    if vmproflogfile.get() < 0:
        vmproflogfile.set(
            os.open("SqueakProfile.log", os.O_RDWR | os.O_CREAT | os.O_BINARY, 0666)
        )
        try:
            rvmprof.enable(logfile.get(), DEFAULT_PERIOD)
        except rvmprof.VMProfError as e:
            print e.msg
            raise PrimitiveFailedError
        jitlogfile.set(
            os.open("SqueakJitlog.log", os.O_RDWR | os.O_CREAT | os.O_BINARY, 0666)
        )
        try:
            rjitlog.enable_jitlog(fileno)
        except rjitlog.JitlogError as e:
            print e.msg
            raise PrimitiveFailedError
    return w_rcvr
