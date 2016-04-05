import py, os, math, time
from spyvm import model, model_display, storage_contexts, constants, primitives, wrapper, display
from spyvm.primitives import prim_table, PrimitiveFailedError
import spyvm.plugins.socket as socket
from rpython.rlib.rfloat import isinf, isnan
from rpython.rlib.rarithmetic import intmask, r_uint
from rpython.rtyper.lltypesystem import lltype, rffi
from .util import create_space, copy_to_module, cleanup_module, TestInterpreter, very_slow_test
from .test_interpreter import run_with_faked_primitive_methods
from .test_primitives import MockFrame, mock

def setup_module():
    space = create_space(bootstrap = True)
    space.set_system_attribute(constants.SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, "IMAGENAME")
    wrap = space.w
    bootstrap_class = space.bootstrap_class
    new_frame = space.make_frame
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

IMAGENAME = "anImage.image"

def _prim(space, name, module, stack, context = None):
    interp, w_frame, argument_count = mock(space, stack, context)
    orig_stack = list(w_frame.as_context_get_shadow(space).stack())
    prim_meth = model.W_PreSpurCompiledMethod(space, 0, header=17045052)
    prim_meth._primitive = primitives.EXTERNAL_CALL
    prim_meth.argsize = argument_count - 1
    descr = space.wrap_list([space.wrap_string(module), space.wrap_string(name)])
    prim_meth.literalatput0(space, 1, descr)
    def call():
        prim_table[primitives.EXTERNAL_CALL](interp, w_frame.as_context_get_shadow(space), argument_count-1, prim_meth)
    return w_frame, orig_stack, call

def prim(name, module=None, stack = None, context = None):
    if module is None: module = "SocketPlugin"
    if stack is None: stack = [space.w_nil]
    w_frame, orig_stack, call = _prim(space, name, module, stack, context)
    call()
    res = w_frame.as_context_get_shadow(space).pop()
    s_frame = w_frame.as_context_get_shadow(space)
    assert not s_frame.stackdepth() - s_frame.tempsize()  # check args are consumed
    return res

def prim_fails(name, module, stack):
    w_frame, orig_stack, call = _prim(name, module, stack)
    with py.test.raises(PrimitiveFailedError):
        call()
    assert w_frame.as_context_get_shadow(space).stack() == orig_stack


def test_vmdebugging():
    assert prim("isRSqueak", "VMDebugging") is space.w_true


def test_resolver_start_lookup():
    assert prim("primitiveResolverStartNameLookup", "SocketPlugin",
                [space.w_nil, space.wrap_string("google.com")]) == space.w_nil

def test_resolver_lookup_result():
    assert prim("primitiveResolverStartNameLookup", "SocketPlugin",
                [space.w_nil, space.wrap_string("google.com")]) == space.w_nil
    w_res = prim("primitiveResolverNameLookupResult", "SocketPlugin")
    assert isinstance(w_res, model.W_BytesObject)


def test_socket_create():
    assert isinstance(prim("primitiveSocketCreate3Semaphores", "SocketPlugin",
                           [space.w_nil, 2, 0, 8000, 8000, 13, 14, 15]), socket.W_SocketHandle)
    assert isinstance(prim("primitiveSocketCreate3Semaphores", "SocketPlugin",
                           [space.w_nil, 0, 0, 8000, 8000, 13, 14, 15]), socket.W_SocketHandle)

def test_socket_status():
    handle = prim("primitiveSocketCreate3Semaphores", "SocketPlugin",
                  [space.w_nil, 2, 0, 8000, 8000, 13, 14, 15])
    assert prim("primitiveSocketConnectionStatus", "SocketPlugin",
                [space.w_nil, handle]).value == 0
    assert prim("primitiveSocketConnectionStatus", "SocketPlugin",
                [space.w_nil, 3200]).value == -1

def test_socket_connect():
    handle = prim("primitiveSocketCreate3Semaphores", "SocketPlugin",
                  [space.w_nil, 2, 0, 8000, 8000, 13, 14, 15])
    prim("primitiveResolverStartNameLookup", "SocketPlugin",
         [space.w_nil, space.wrap_string("google.com")])
    w_host = prim("primitiveResolverNameLookupResult", "SocketPlugin")
    assert prim("primitiveSocketConnectToPort", "SocketPlugin",
                [space.w_nil, handle, w_host, space.wrap_int(80)])
    assert prim("primitiveSocketConnectionStatus", "SocketPlugin",
                [space.w_nil, handle]).value == 2

def test_socket_ready():
    handle = prim("primitiveSocketCreate3Semaphores", "SocketPlugin",
                  [space.w_nil, 2, 0, 8000, 8000, 13, 14, 15])
    prim("primitiveResolverStartNameLookup", "SocketPlugin",
         [space.w_nil, space.wrap_string("google.com")])
    w_host = prim("primitiveResolverNameLookupResult", "SocketPlugin")
    assert prim("primitiveSocketConnectToPort", "SocketPlugin",
                [space.w_nil, handle, w_host, space.wrap_int(80)])
    assert prim("primitiveSocketConnectionStatus", "SocketPlugin",
                [space.w_nil, handle]).value == 2
    time.sleep(0.5)
    assert prim("primitiveSocketReceiveDataAvailable", "SocketPlugin",
                [space.w_nil, handle]) == space.w_false

_http_get = """
GET / HTTP/1.1
User-Agent: curl/7.37.1
Host: www.google.de
Accept: */*

"""
def test_socket_send_and_read_into():
    handle = prim("primitiveSocketCreate3Semaphores", "SocketPlugin",
                  [space.w_nil, 2, 0, 8000, 8000, 13, 14, 15])
    prim("primitiveResolverStartNameLookup", "SocketPlugin",
         [space.w_nil, space.wrap_string("google.com")])
    w_host = prim("primitiveResolverNameLookupResult", "SocketPlugin")
    assert prim("primitiveSocketConnectToPort", "SocketPlugin",
                [space.w_nil, handle, w_host, space.wrap_int(80)])
    assert prim("primitiveSocketConnectionStatus", "SocketPlugin",
                [space.w_nil, handle]).value == 2
    assert prim("primitiveSocketSendDataBufCount", "SocketPlugin",
                [space.w_nil, handle, space.wrap_string(_http_get),
                 space.wrap_int(1), space.wrap_int(len(_http_get))]).value == len(_http_get)
    time.sleep(0.5)
    assert prim("primitiveSocketReceiveDataAvailable", "SocketPlugin",
                [space.w_nil, handle]) == space.w_true
    w_str = space.wrap_string("_hello")
    assert prim("primitiveSocketReceiveDataBufCount", "SocketPlugin",
                [space.w_nil, handle, w_str, space.wrap_int(2), space.wrap_int(5)]).value == 5
    assert w_str.unwrap_string(None) == "_HTTP/"

def test_socket_destroy():
    handle = prim("primitiveSocketCreate3Semaphores", "SocketPlugin",
                  [space.w_nil, 2, 0, 8000, 8000, 13, 14, 15])
    assert prim("primitiveSocketDestroy", "SocketPlugin",
                [space.w_nil, handle]).value == -1

def test_WebClient_dance():
    assert prim("primitiveResolverStatus").value == 1
    assert prim("primitiveResolverStartNameLookup",
                stack=[space.w_nil, space.wrap_string('squeak.org')]) is space.w_nil
    assert prim("primitiveResolverStatus").value == 1
    w_ip = prim("primitiveResolverNameLookupResult")
    assert prim("primitiveResolverStatus").value == 1
    w_handle = prim("primitiveSocketCreate3Semaphores",
                    stack=[space.w_nil, space.wrap_int(0), space.wrap_int(0), space.wrap_int(8000), space.wrap_int(8000), space.wrap_int(18), space.wrap_int(17), space.wrap_int(16)])
    assert prim("primitiveSocketConnectionStatus",
                stack=[space.w_nil, w_handle]).value == 0
    assert prim("primitiveResolverStatus").value == 1
    assert prim("primitiveSocketConnectionStatus",
                stack=[space.w_nil, w_handle]).value == 0
    prim("primitiveSocketConnectToPort",
         stack=[space.w_nil, w_handle, w_ip, space.wrap_int(80)])
    assert prim("primitiveSocketConnectionStatus",
                stack=[space.w_nil, w_handle]).value == 2
    assert prim("primitiveSocketSendDone",
                stack=[space.w_nil, w_handle]) == space.w_true
    assert prim("primitiveSocketSendDataBufCount",
         stack=[space.w_nil, w_handle, space.wrap_string("""GET / HTTP/1.1
User-Agent: WebClient/1.5 (WebClient-Core-ul.100; Squeak4.5-14888; nil)
Accept-Encoding: gzip
Host: squeak.org

""".replace("\n", "\r\n")), space.wrap_int(1), space.wrap_int(132)]).value == 132
    time.sleep(0.5)
    assert prim("primitiveSocketConnectionStatus",
                stack=[space.w_nil, w_handle]).value == 2
    assert prim("primitiveSocketReceiveDataAvailable",
                stack=[space.w_nil, w_handle]) is space.w_true
    w_data = space.wrap_string("".join([" "] * 4096))
    w_result = prim("primitiveSocketReceiveDataBufCount",
                           stack=[space.w_nil, w_handle, w_data, space.wrap_int(1), space.wrap_int(4096)])
    assert isinstance(w_result, model.W_SmallInteger)
    while w_result.value == 4096:
        w_result = prim("primitiveSocketReceiveDataBufCount",
                        stack=[space.w_nil, w_handle, w_data, space.wrap_int(1), space.wrap_int(4096)])
    assert prim("primitiveSocketReceiveDataAvailable",
                stack=[space.w_nil, w_handle]) is space.w_false
    w_state = prim("primitiveSocketConnectionStatus",
                   stack=[space.w_nil, w_handle])
    # either still connected or OtherEndClosed
    assert w_state.value == 2 or w_state.value == 3
    prim("primitiveSocketCloseConnection",
                stack=[space.w_nil, w_handle])
    assert prim("primitiveSocketConnectionStatus",
                stack=[space.w_nil, w_handle]).value == 0
    prim("primitiveSocketDestroy",
         stack=[space.w_nil, w_handle])
    assert prim("primitiveSocketConnectionStatus",
                stack=[space.w_nil, w_handle]).value == -1
