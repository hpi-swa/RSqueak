import errno

from rsqueakvm import error
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.plugin import Plugin
from rsqueakvm.util.cells import Cell
from rsqueakvm.util.system import IS_SHELL, IS_WINDOWS

from rpython.rlib import rsocket, _rsocket_rffi, objectmodel


if IS_WINDOWS:
    def non_blocking_recv(self, count):
        self.socket._setblocking(False)
        try:
            return self.socket.recv(count)
        finally:
            self.socket._setblocking(True)
else:
    # on unix, we just use the flag, avoiding the extra fcntl calls
    def non_blocking_recv(self, count):
        return self.socket.recv(count, _rsocket_rffi.MSG_DONTWAIT)


ResolverUninitialized = 0
ResolverReady = 1
ResolverBusy = 2
ResolverError = 3


class SocketPlugin(Plugin):
    _attrs_ = ["fds", "sockets", "last_lookup"]

    def __init__(self):
        Plugin.__init__(self)
        self.last_lookup = Cell(None)

    @staticmethod
    def startup(space, argv):
        from rpython.rlib.rsocket import rsocket_startup
        rsocket_startup()

    # cannot overload call (plugins are PBCs) so we decorate the decorator
    @objectmodel.not_rpython
    def expose_primitive(self, wrap_func=None, **kwargs):
        if not self.is_enabled():
            return lambda x: x  # do not install primitives when disabled
        original_decorator = Plugin.expose_primitive(self, wrap_func=wrap_func, **kwargs)
        def decorator(func):
            original_decorator(func)
            wrapped = self.primitives[func.func_name]
            def catchall(interp, s_frame, argcount, w_method=None):
                try:
                    return wrapped(interp, s_frame, argcount, w_method=w_method)
                except rsocket.SocketError as e:
                    print "!!!SOCKET ERROR!!!"
                    print e.get_msg()
                    print "!!!SOCKET ERROR!!!"
                    raise error.PrimitiveFailedError
            catchall.func_name = "catchall_" + wrapped.func_name
            self.primitives[func.func_name] = catchall
            return func
        return decorator

    def set_last_lookup(self, v):
        self.last_lookup.set(v)

    def get_last_lookup(self):
        return self.last_lookup.get()

    def is_socket(self, space, w_int):
        return isinstance(w_int, W_SocketHandle)


if IS_SHELL:
    def wrappedcall(self, name, interp, s_frame, argcount, w_method):
        import time
        # sys.stdout.write("%s(%s): " % (name, s_frame.peek_n(argcount)))
        print name
        if "Socket" in name:
            time.sleep(0.5)
        r = Plugin.call(self, name, interp, s_frame, argcount, w_method)
        # print r
        return r
    SocketPlugin.call = wrappedcall


plugin = SocketPlugin()
InvalidSocket = -1
Unconnected = 0
WaitingForConnection = 1
Connected = 2
OtherEndClosed = 3
ThisEndClosed = 4


class W_SocketHandle(W_AbstractObjectWithIdentityHash):
    _attrs_ = ["socket", "state", "family", "socketType"]
    repr_classname = "W_SocketHandle"

    def __init__(self, family, socketType):
        self.socket = None
        self.state = Unconnected
        self.family = family
        self.socketType = socketType
        self.make_socket()

    def make_socket(self):
        try:
            self.socket = rsocket.RSocket(family=self.family, proto=self.socketType)
        except rsocket.CSocketError:
            raise error.PrimitiveFailedError
        self.socket.setblocking(False)

    def isipv4(self):
        return self.family == rsocket.AF_INET

    def isipv6(self):
        return self.family == rsocket.AF_INET6

    def getclass(self, space):
        return space.w_SmallInteger

    def guess_classname(self):
        return "SocketHandle"

    def connect(self, w_bytes, port):
        try:
            inet = rsocket.INETAddress(w_bytes.unwrap_string(None), port)
        except rsocket.GAIError:
            try:
                inet = rsocket.INET6Address(w_bytes.unwrap_string(None), port)
                self.family = rsocket.AF_INET6
                self.make_socket()
            except rsocket.GAIError:
                raise error.PrimitiveFailedError
        self.socket.setblocking(True)
        try:
            self.socket.connect(inet)
        except rsocket.SocketError:
            raise error.PrimitiveFailedError
        finally:
            self.socket.setblocking(False)
        self.state = Connected

    def can_read(self):
        if self.state == Connected:
            try:
                r = self.socket.recv(1, rsocket.MSG_PEEK)
            except rsocket.CSocketError, e:
                if e.errno == errno.EAGAIN or e.errno == errno.EWOULDBLOCK:
                    return False
                raise
            if len(r) == 0:
                self.state = OtherEndClosed
                return False
            else:
                return True
        return False

    def recv(self, count):
        try:
            data = non_blocking_recv(self, count)
        except rsocket.CSocketError:
            raise error.PrimitiveFailedError
        if len(data) == 0:
            self.state = OtherEndClosed
        return data

    def send(self, data):
        return self.socket.send(data)

    def close(self):
        if (self.state == Connected or
            self.state == OtherEndClosed or
                self.state == WaitingForConnection):
            self.socket.close()
            self.state = Unconnected

    def destroy(self):
        if self.state != InvalidSocket:
            self.state = InvalidSocket

    def __del__(self):
        self.close()


def ensure_socket(w_socket):
    if not isinstance(w_socket, W_SocketHandle):
        raise error.PrimitiveFailedError
    else:
        return w_socket


@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetNameInfoHostResult(interp, s_frame, argcount):  # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetNameInfoHostResult"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetNameInfoServiceSize(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetNameInfoServiceSize"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetNameInfoServiceResult(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetNameInfoServiceResult"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverStartAddressLookup(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverStartAddressLookup"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetAddressInfo(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetAddressInfo"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverError(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverError"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetAddressInfoSize(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetAddressInfoSize"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverHostNameSize(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverHostNameSize"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverAbortLookup(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverAbortLookup"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetAddressInfoType(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetAddressInfoType"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverAddressLookupResult(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverAddressLookupResult"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetAddressInfoFamily(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetAddressInfoFamily"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetAddressInfoNext(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetAddressInfoNext"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetNameInfo(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetNameInfo"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetNameInfoHostSize(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetNameInfoHostSize"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverHostNameResult(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverHostNameResult"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetAddressInfoResult(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetAddressInfoResult"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverLocalAddress(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverLocalAddress"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveResolverGetAddressInfoProtocol(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveResolverGetAddressInfoProtocol"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=[object])
def primitiveHasSocketAccess(interp, s_frame, w_rcvr):
    # if security plugin forbids it, this should return false
    return interp.space.w_true

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketAddressSetPort(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketAddressSetPort"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketAddressGetPort(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketAddressGetPort"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketReceiveUDPDataBufCount(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketReceiveUDPDataBufCount"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketSetOptions(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketSetOptions"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketRemoteAddressSize(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketRemoteAddressSize"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketConnectTo(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketConnectTo"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketLocalAddressSize(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketLocalAddressSize"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketRemoteAddress(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketRemoteAddress"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketLocalPort(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketLocalPort"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketError(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketError"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketBindTo(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketBindTo"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketAbortConnection(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketAbortConnection"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketListenWithBacklog(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketListenWithBacklog"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketGetOptions(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketGetOptions"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketSendUDPDataBufCount(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketSendUDPDataBufCount"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketLocalAddressResult(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketLocalAddressResult"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketLocalAddress(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketLocalAddress"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketListenWithOrWithoutBacklog(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketListenWithOrWithoutBacklog"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketAccept3Semaphores(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketAccept3Semaphores"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketRemoteAddressResult(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketRemoteAddressResult"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketListenOnPortBacklogInterface(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketListenOnPortBacklogInterface"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=[object, object])
def primitiveSocketCloseConnection(interp, s_frame, w_rcvr, w_handle):
    w_socket = ensure_socket(w_handle)
    try:
        w_socket.close()
    except rsocket.SocketError:
        raise error.PrimitiveFailedError
    return interp.space.w_nil

@plugin.expose_primitive(unwrap_spec=None)
def primitiveSocketRemotePort(interp, s_frame, argcount): # pragma: no cover
    if not objectmodel.we_are_translated():
        import pdb; pdb.set_trace()
        raise error.PrimitiveFailedError
    else:
        print "Missing Socket primitive primitiveSocketRemotePort"
        raise error.PrimitiveFailedError

@plugin.expose_primitive(unwrap_spec=[object, int, int, int, int, int, int, int])
def primitiveSocketCreate3Semaphores(interp, s_frame, w_rcvr, netType, socketType, rcvBufSize, sendBufSize, sema, readSema, writeSema):
    if netType == 0: # undefined
        netType = rsocket.AF_INET
    return W_SocketHandle(netType, socketType)

@plugin.expose_primitive(unwrap_spec=[object, object])
def primitiveSocketConnectionStatus(interp, s_frame, w_rcvr, w_socket):
    if not isinstance(w_socket, W_SocketHandle):
        return interp.space.wrap_int(InvalidSocket)
    else:
        return interp.space.wrap_int(w_socket.state)

@plugin.expose_primitive(unwrap_spec=[object, object, object, int])
def primitiveSocketConnectToPort(interp, s_frame, w_rcvr, w_handle, w_hostaddr, port):
    w_socket = ensure_socket(w_handle)
    if not isinstance(w_hostaddr, W_BytesObject):
        raise error.PrimitiveFailedError
    w_socket.connect(w_hostaddr, port)
    return interp.space.w_nil

@plugin.expose_primitive(unwrap_spec=[object, object])
def primitiveSocketSendDone(interp, s_frame, w_rcvr, fd):
    return interp.space.w_true

@plugin.expose_primitive(unwrap_spec=[object, object, str, int, int])
def primitiveSocketSendDataBufCount(interp, s_frame, w_rcvr, w_handle, data, start, count):
    w_socket = ensure_socket(w_handle)
    s = start - 1
    if s < 0:
        raise error.PrimitiveFailedError
    e = s + count
    if e > len(data):
        raise error.PrimitiveFailedError
    assert e >= 0
    res = w_socket.send(data[s:e])
    return interp.space.wrap_int(res)

@plugin.expose_primitive(unwrap_spec=[object, object])
def primitiveSocketReceiveDataAvailable(interp, s_frame, w_rcvr, w_handle):
    w_socket = ensure_socket(w_handle)
    if w_socket.can_read():
        return interp.space.w_true
    else:
        return interp.space.w_false

@plugin.expose_primitive(unwrap_spec=[object, object, object, int, int])
def primitiveSocketReceiveDataBufCount(interp, s_frame, w_rcvr, w_handle, w_target, start, count):
    w_socket = ensure_socket(w_handle)
    if start + count - 1 > w_target.size():
        raise error.PrimitiveFailedError
    if not isinstance(w_target, W_BytesObject):
        raise error.PrimitiveFailedError
    try:
        data = w_socket.recv(count)
    except rsocket.SocketError:
        return interp.space.wrap_int(0)
    for idx, char in enumerate(data):
        w_target.setchar(idx + start - 1, char)
    return interp.space.wrap_int(len(data))

@plugin.expose_primitive(unwrap_spec=[object, object])
def primitiveSocketDestroy(interp, s_frame, w_rcvr, w_handle):
    w_socket = ensure_socket(w_handle)
    try:
        w_socket.destroy()
    except rsocket.SocketError:
        raise error.PrimitiveFailedError
    return interp.space.wrap_int(w_socket.state)

@plugin.expose_primitive(unwrap_spec=[object, object])
def primitiveInitializeNetwork(interp, s_frame, w_rcvr, w_semaphore):
    return w_rcvr

@plugin.expose_primitive(unwrap_spec=[object])
def primitiveResolverStatus(interp, s_frame, w_rcvr):
    return interp.space.wrap_int(ResolverReady)

@plugin.expose_primitive(unwrap_spec=[object, str])
def primitiveResolverStartNameLookup(interp, s_frame, w_rcvr, hostname):
    try:
        host = rsocket.INETAddress(hostname, 80).get_host()
        plugin.set_last_lookup(host)
    except rsocket.GAIError, e:
        print "SocketError: %s" % e.get_msg()
        plugin.set_last_lookup(None)
    return interp.space.w_nil

@plugin.expose_primitive(unwrap_spec=[object])
def primitiveResolverNameLookupResult(interp, s_frame, w_rcvr):
    inet = plugin.get_last_lookup()
    if inet is None:
        return interp.space.w_nil
    else:
        return interp.space.wrap_string(inet)
