import py
import time

from rsqueakvm import constants
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.primitives import prim_table
from rsqueakvm.primitives.bytecodes import EXTERNAL_CALL
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins import socket as socket

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
    prim_meth = W_PreSpurCompiledMethod(space, 0, header=17045052)
    prim_meth._primitive = EXTERNAL_CALL
    prim_meth.argsize = argument_count - 1
    descr = space.wrap_list([space.wrap_string(module), space.wrap_string(name)])
    prim_meth.literalatput0(space, 1, descr)
    def call():
        prim_table[EXTERNAL_CALL](interp, w_frame.as_context_get_shadow(space), argument_count-1, prim_meth)
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
    assert isinstance(w_res, W_BytesObject)


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
