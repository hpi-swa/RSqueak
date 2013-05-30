from spyvm import model, error
from spyvm.plugins.plugin import Plugin


DebuggingPlugin = Plugin()

DebuggingPlugin.userdata['stop_ui'] = False
def stop_ui_process():
    DebuggingPlugin.userdata['stop_ui'] = True

@DebuggingPlugin.expose_primitive(unwrap_spec=[object])
def trace(interp, s_frame, w_rcvr):
    interp.trace = True
    return w_rcvr

@DebuggingPlugin.expose_primitive(unwrap_spec=[object])
def untrace(interp, s_frame, w_rcvr):
    interp.trace = False
    return w_rcvr

@DebuggingPlugin.expose_primitive(unwrap_spec=[object])
def halt(interp, s_frame, w_rcvr):
    from rpython.rlib.objectmodel import we_are_translated
    from spyvm.error import Exit

    print s_frame.print_stack()
    if not we_are_translated():
        import pdb; pdb.set_trace()
    else:
        print s_frame
        raise Exit('Halt is not well defined when translated.')
    return w_rcvr

@DebuggingPlugin.expose_primitive(unwrap_spec=[object])
def isRSqueak(interp, s_frame, w_rcvr):
    return interp.space.w_true

@DebuggingPlugin.expose_primitive(unwrap_spec=[object])
def isVMTranslated(interp, s_frame, w_rcvr):
    from rpython.rlib.objectmodel import we_are_translated
    if we_are_translated():
        return interp.space.w_true
    else:
        return interp.space.w_false

@DebuggingPlugin.expose_primitive(unwrap_spec=[object, object])
def debugPrint(interp, s_frame, w_rcvr, w_string):
    if not isinstance(w_string, model.W_BytesObject):
        raise error.PrimitiveFailedError()
    print w_string.as_string().replace('\r', '\n')
    return w_rcvr

@DebuggingPlugin.expose_primitive(unwrap_spec=[object])
def stopUIProcess(interp, s_frame, w_rcvr):
    if DebuggingPlugin.userdata.get('stop_ui', False):
        return interp.space.w_true
    else:
        return interp.space.w_false
