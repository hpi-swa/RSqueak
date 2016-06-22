import os

from rsqueakvm import error
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.util.system import IS_WINDOWS


VMDebugging = Plugin()

VMDebugging.userdata['stop_ui'] = False
def stop_ui_process():
    VMDebugging.userdata['stop_ui'] = True

# @VMDebugging.expose_primitive(unwrap_spec=[object])
# def trace(interp, s_frame, w_rcvr):
#     interp.trace = True
#     return w_rcvr

# @VMDebugging.expose_primitive(unwrap_spec=[object])
# def untrace(interp, s_frame, w_rcvr):
#     interp.trace = False
#     return w_rcvr


if IS_WINDOWS:
    def fork():
        raise NotImplementedError("fork on windows")
else:
    fork = os.fork


@VMDebugging.expose_primitive(unwrap_spec=[object])
def trace_proxy(interp, s_frame, w_rcvr):
    interp.trace_proxy.activate()
    return w_rcvr

@VMDebugging.expose_primitive(unwrap_spec=[object])
def untrace_proxy(interp, s_frame, w_rcvr):
    interp.trace_proxy.deactivate()
    return w_rcvr

@VMDebugging.expose_primitive(unwrap_spec=[object])
def halt(interp, s_frame, w_rcvr):
    from rpython.rlib.debug import attach_gdb
    print s_frame.print_stack()
    raise NotImplementedError
    # attach_gdb()

@VMDebugging.expose_primitive(unwrap_spec=[object])
def isRSqueak(interp, s_frame, w_rcvr):
    return interp.space.w_true

@VMDebugging.expose_primitive(unwrap_spec=[object])
def isVMTranslated(interp, s_frame, w_rcvr):
    from rpython.rlib.objectmodel import we_are_translated
    if we_are_translated():
        return interp.space.w_true
    else:
        return interp.space.w_false

@VMDebugging.expose_primitive(unwrap_spec=[object, object])
def debugPrint(interp, s_frame, w_rcvr, w_string):
    if not isinstance(w_string, W_BytesObject):
        raise error.PrimitiveFailedError()
    print interp.space.unwrap_string(w_string).replace('\r', '\n')
    return w_rcvr

@VMDebugging.expose_primitive(unwrap_spec=[object])
def stopUIProcess(interp, s_frame, w_rcvr):
    if VMDebugging.userdata.get('stop_ui', False):
        return interp.space.w_true
    else:
        return interp.space.w_false
