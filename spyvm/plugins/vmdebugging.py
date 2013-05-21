from spyvm import model, error
from spyvm.plugins.plugin import Plugin


DebuggingPlugin = Plugin()


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

    if not we_are_translated():
        import pdb; pdb.set_trace()
    else:
        print s_frame.print_stack()
        print s_frame
        raise Exit('Halt is not well defined when translated.')
    return w_rcvr

@DebuggingPlugin.expose_primitive(unwrap_spec=[object, object])
def debugPrint(interp, s_frame, w_rcvr, w_string):
    if not isinstance(w_string, model.W_BytesObject):
        raise error.PrimitiveFailedError()
    print w_string.as_string()
    return w_rcvr
