import os
import py
import time
import base64
import socket as pysocket
import sys

from rsqueakvm import constants
from rsqueakvm.model.compiled_methods import W_PreSpurCompiledMethod
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.primitives import prim_table
from rsqueakvm.primitives.constants import EXTERNAL_CALL
from rsqueakvm.error import PrimitiveFailedError
from rsqueakvm.plugins import socket_plugin as socket

from .util import create_space, copy_to_module, cleanup_module
from .test_primitives import mock
from . import squeakssl_data

from rpython.rlib import ropenssl
ropenssl.init_ssl()
ropenssl.init_digests()

def setup_module():
    space = create_space(bootstrap = True)
    space.set_system_attribute(constants.SYSTEM_ATTRIBUTE_IMAGE_NAME_INDEX, "IMAGENAME")
    w = space.w
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
    if module is None: module = "SqueakSSL"
    if stack is None:
        stack = [space.w_nil]
    else:
        stack = [space.w_nil] + stack
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

def fix(name):
    f = getattr(squeakssl_data, name)
    return w(base64.standard_b64decode(f))

@py.test.mark.skipif(os.name == 'nt' or sys.platform == 'darwin')
def test_https_connect():
    import os
    w_handle = prim("primitiveCreate")
    prim("primitiveSetIntProperty", stack=[w_handle, w(1), w(2)])
    assert w_handle.loglevel == 2
    prim("primitiveSetStringProperty", stack=[w_handle, w(2), w("www.google.com")])
    assert w_handle.servername == "www.google.com"
    w_result = prim("primitiveConnect",
            stack=[
                w_handle,
                w(""),
                w(1),
                w(0),
                fix("outbuf1")])
    assert w_result.value == 320
    w_result = prim("primitiveConnect",
            stack=[
                w_handle,
                fix("inbuf2"),
                w(1),
                w(3501),
                fix("outbuf2")])
    assert w_result.value == -1
    return
    # those cannot be mocked
    assert w_result.value == 126
    w_result = prim("primitiveConnect",
            stack=[
                w_handle,
                fix("inbuf3"),
                w(1),
                w(262),
                fix("outbuf3")])
    assert w_result.value == 0

@py.test.mark.skipif(sys.platform == 'darwin')
def test_http_real():
    s = pysocket.socket(pysocket.AF_INET, pysocket.SOCK_STREAM)
    s.connect((pysocket.gethostbyname("www.google.com"), 443))
    w_handle = prim("primitiveCreate")
    prim("primitiveSetIntProperty", stack=[w_handle, w(1), w(2)])
    prim("primitiveSetStringProperty", stack=[w_handle, w(2), w("localhost:4443")])
    w_out = fix("outbuf1")
    w_result = prim("primitiveConnect",
                    stack=[
                        w_handle,
                        w(""),
                        w(1),
                        w(0),
                        w_out])
    assert w_result.value > 0
    s.sendall(space.unwrap_string(w_out)[0:w_result.value])
    inbuf = s.recv(4096)
    w_result = prim("primitiveConnect",
                    stack=[
                        w_handle,
                        w(inbuf),
                        w(1),
                        w(len(inbuf)),
                        w_out])
    assert w_result.value > 0
    s.sendall(space.unwrap_string(w_out)[0:w_result.value])
    inbuf = s.recv(4096)
    w_result = prim("primitiveConnect",
                    stack=[
                        w_handle,
                        w(inbuf),
                        w(1),
                        w(len(inbuf)),
                        w_out])
    assert w_result.value == 0
    _get = """GET / HTTP/1.1
User-Agent: WebClient/1.5
Accept-Encoding: gzip
Host: www.google.com

""".replace("\n", "\r\n")
    w_result = prim("primitiveEncrypt",
                    stack=[
                        w_handle,
                        w(_get),
                        w(1),
                        w(len(_get)),
                        w_out])
    assert w_result.value > 0
    s.sendall(space.unwrap_string(w_out)[0:w_result.value])
    inbuf = s.recv(4096)
    w_result = prim("primitiveDecrypt",
                    stack=[
                        w_handle,
                        w(inbuf),
                        w(1),
                        w(len(inbuf)),
                        w_out])
    assert w_result.value > 0
    http_response = space.unwrap_string(w_out)[0:w_result.value]
    assert http_response.startswith("HTTP/1.1")
