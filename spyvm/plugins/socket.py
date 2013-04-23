from rpython.rlib import rsocket

from spyvm import model, error
from spyvm.plugins.plugin import Plugin


SocketPlugin = Plugin()

def is_squeak_socket(w_socket_handle):
    return not isinstance(w_socket_handle, W_SocketHandle)


@SocketPlugin.expose_primitive(unwrap_spec=[object, object])
def primitiveSocketConnectionStatus(interp, s_frame, w_rcvr, w_socket_handle):
    """ "Socket Status Values"
        InvalidSocket := -1.
        Unconnected := 0.
        WaitingForConnection := 1.
        Connected := 2.
        OtherEndClosed := 3.
        ThisEndClosed := 4. """
    if is_squeak_socket(w_socket_handle):
        return interp.space.wrap_int(-1)
    else:
        return interp.space.wrap_int(-1) # for now ...


@SocketPlugin.expose_primitive(unwrap_spec=[object])
def primitiveResolverStatus(interp, s_frame, w_rcvr):
    """ "Resolver Status Values"
        ResolverUninitialized := 0.     "network is not initialized"
        ResolverReady := 1.             "resolver idle, last request succeeded"
        ResolverBusy := 2.              "lookup in progress"
        ResolverError := 3.             "resolver idle, last request failed"
    """
    if not SocketPlugin.userdata.get("thisNetSession", None):
        return interp.space.wrap_int(0)
    elif SocketPlugin.userdata.get("lastError", 0) != 0:
        return interp.space.wrap_int(3)
    else:
        return interp.space.wrap_int(1)


@SocketPlugin.expose_primitive(unwrap_spec=[object, object])
def primitiveInitializeNetwork(interp, s_frame, w_rcvr, w_semaphore):
    """
    Initialize the network drivers on platforms that need it.

    ... Certainly sir ...

    Note: some platforms (e.g., Mac) only allow
    only one name lookup query at a time, so a manager process should
    be used to serialize resolver lookup requests

    ... IGNORE ME! ...
    """
    return w_rcvr


class W_SocketHandle(model.W_WordsObject):
    pass
