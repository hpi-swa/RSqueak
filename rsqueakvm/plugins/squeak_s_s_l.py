from rsqueakvm import error
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.primitives import index1_0
from rsqueakvm.plugins.plugin import Plugin, PluginStartupScripts

from rpython.rlib import ropenssl
from rpython.rtyper.lltypesystem import rffi, lltype


ropenssl.ssl_external('SSL_set_bio', [ropenssl.SSL, ropenssl.BIO, ropenssl.BIO], lltype.Void)
ropenssl.ssl_external('BIO_read', [ropenssl.BIO, rffi.CCHARP, rffi.INT], rffi.INT)
ropenssl.ssl_external('BIO_write', [ropenssl.BIO, rffi.CCHARP, rffi.INT], rffi.INT)
# XXX: these are taken from bio.h
PY_BIO_CTRL_PENDING = 10


def startup(space, argv):
    ropenssl.init_ssl()
    ropenssl.init_digests()
PluginStartupScripts.append(startup)


SqueakSSL = Plugin()
SSL_UNUSED = 0
SSL_NOT_CONNECTED = 0
SSL_ACCEPTING = 1
SSL_CONNECTING = 2
SSL_CONNECTED = 3

PROP_VERSION = 0
PROP_LOGLEVEL = 1
PROP_SSLSTATE = 2
PROP_CERTSTATE = 3
PROP_PEERNAME = 0
PROP_CERTNAME = 1
PROP_SERVERNAME = 2


class W_SSLHandle(W_AbstractObjectWithIdentityHash):
    _attrs_ = ["ctx", "ssl", "readbio", "writebio",
               "state", "servername", "peername", "certname", "certflags",
               "wasclosed"]
    _immutable_fields_ = ["ctx", "ssl", "readbio", "writebio"]

    def __init__(self):
        self.state = SSL_UNUSED
        self.certflags = 0
        self.peername = ""
        self.servername = ""
        self.certname = ""
        self.wasclosed = False
        self.ctx = ropenssl.libssl_SSL_CTX_new(ropenssl.libssl_SSLv23_method())
        ropenssl.libssl_SSL_CTX_set_options(self.ctx, ropenssl.SSL_OP_NO_SSLv2 | ropenssl.SSL_OP_NO_SSLv3)
        ropenssl.libssl_SSL_CTX_set_cipher_list(self.ctx, "!ADH:HIGH:MEDIUM:@STRENGTH")
        ropenssl.libssl_SSL_CTX_set_default_verify_paths(self.ctx)
        self.ssl = ropenssl.libssl_SSL_new(self.ctx)
        self.readbio = ropenssl.libssl_BIO_new(ropenssl.libssl_BIO_s_mem())
        self.writebio = ropenssl.libssl_BIO_new(ropenssl.libssl_BIO_s_mem())
        # todo: set_close to BIO_CLOSE
        ropenssl.libssl_SSL_set_bio(self.ssl, self.readbio, self.writebio)

    def getclass(self, space):
        return space.w_SmallInteger

    def close(self):
        self.wasclosed = True
        ropenssl.libssl_BIO_free(self.readbio)
        ropenssl.libssl_BIO_free(self.writebio)
        ropenssl.libssl_SSL_CTX_free(self.ctx)
        # ropenssl.libssl_SSL_free(self.ssl)


def ensured_handle(w_obj):
    if isinstance(w_obj, W_SSLHandle):
        if w_obj.wasclosed == False:
            return w_obj
    raise error.PrimitiveFailedError


def copy_bio_ssl(bio, w_dst, nbytes):
    with rffi.scoped_alloc_buffer(nbytes) as buf:
        ropenssl.libssl_BIO_read(bio, buf.raw, nbytes)
        for idx, char in enumerate(rffi.charp2str(buf.raw)):
            w_dst.setchar(idx, char)


@SqueakSSL.expose_primitive(unwrap_spec=[object])
def primitiveCreate(interp, s_frame, w_rcvr):
    return W_SSLHandle()

@SqueakSSL.expose_primitive(unwrap_spec=[object, object])
def primitiveDestroy(interp, s_frame, w_rcvr, w_sslhandle):
    import pdb; pdb.set_trace()
    ensured_handle(w_sslhandle).close()
    return w_rcvr

# handle, srcBuf, srcLen, dstBuf, dstLen
@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, index1_0, int, object])
def primitiveEncrypt(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):
    import pdb; pdb.set_trace()
    w_sslhandle = ensured_handle(w_handle)
    if w_sslhandle.state != SSL_CONNECTED:
        raise error.PrimitiveFailedError
    num_bytes = ropenssl.libssl_SSL_write(w_sslhandle.ssl, src[start:start+srclen], srclen)
    if num_bytes != srclen:
        raise error.PrimitiveFailedError
    nullp = lltype.nullptr(rffi.VOIDP.TO)
    nbytes = ropenssl.libssl_BIO_ctrl(w_sslhandle.writebio, PY_BIO_CTRL_PENDING, 0, nullp)
    dstlen = w_dst.size()
    if nbytes > dstlen:
        raise error.PrimitiveFailedError
    copy_bio_ssl(w_sslhandle.writebio, w_dst, nbytes)
    return interp.space.wrap_int(nbytes)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, index1_0, int, object])
def primitiveDecrypt(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):
    import pdb; pdb.set_trace()
    w_sslhandle = ensured_handle(w_handle)
    if w_sslhandle.state != SSL_CONNECTED:
        raise error.PrimitiveFailedError
    nbytes = ropenssl.libssl_BIO_write(w_sslhandle.readbio, src[start:start+srclen], srclen)
    if nbytes != srclen:
        raise error.PrimitiveFailedError
    dstlen = w_dst.size()
    with rffi.scoped_alloc_buffer(dstlen) as buf:
        nbytes = ropenssl.libssl_SSL_read(w_sslhandle.ssl, buf.raw, dstlen)
        if nbytes <= 0:
            err = ropenssl.libssl_SSL_get_error(w_sslhandle.ssl, nbytes)
            if (err != ropenssl.SSL_ERROR_WANT_READ and
                err != ropenssl.SSL_ERROR_ZERO_RETURN):
                raise error.PrimitiveFailedError
            else:
                nbytes = 0
        for idx, char in enumerate(rffi.charp2str(buf.raw)):
            w_dst.setchar(idx, char)
    return interp.space.wrap_int(nbytes)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int, str])
def primitiveSetStringProperty(interp, s_frame, w_rcvr, w_handle, propid, value):
    w_sslhandle = ensured_handle(w_handle)
    if propid == PROP_CERTNAME:
        w_sslhandle.certname = value
    elif propid == PROP_SERVERNAME:
        w_sslhandle.servername = value
    else:
        return interp.space.wrap_int(0)
    return interp.space.wrap_int(1)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int])
def primitiveGetStringProperty(interp, s_frame, w_rcvr, w_handle, propid):
    w_sslhandle = ensured_handle(w_handle)
    if propid == PROP_PEERNAME:
        # r = w_sslhandle.peername
        r = "*"
    elif propid == PROP_CERTNAME:
        r = w_sslhandle.certname
    elif propid == PROP_SERVERNAME:
        r = w_sslhandle.servername
    else:
        r = ""
    return interp.space.wrap_string(r)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int, int])
def primitiveSetIntProperty(interp, s_frame, w_rcvr, w_handle, propid, value):
    w_sslhandle = ensured_handle(w_handle)
    return interp.space.wrap_int(0)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int])
def primitiveGetIntProperty(interp, s_frame, w_rcvr, w_handle, propid):
    w_sslhandle = ensured_handle(w_handle)
    if propid == PROP_VERSION:
        r = 2
    elif propid == PROP_CERTSTATE:
        # r = w_sslhandle.certflags
        r = 0
    elif propid == PROP_SSLSTATE:
        r = w_sslhandle.state
    else:
        return interp.space.wrap_int(0)
    return interp.space.wrap_int(r)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, index1_0, int, object])
def primitiveConnect(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):
    import pdb; pdb.set_trace()
    w_sslhandle = ensured_handle(w_handle)
    if w_sslhandle.state != SSL_UNUSED and w_sslhandle.state != SSL_CONNECTING:
        raise error.PrimitiveFailedError
    if w_sslhandle.state == SSL_UNUSED:
        w_sslhandle.state = SSL_CONNECTING
        ropenssl.libssl_SSL_set_connect_state(w_sslhandle.ssl)
    n = ropenssl.libssl_BIO_write(w_sslhandle.readbio, src[start:start+srclen], srclen)
    if n < srclen:
        raise error.PrimitiveFailedError
    if n < 0:
        raise error.PrimitiveFailedError
    if w_sslhandle.servername:
        ropenssl.libssl_SSL_set_tlsext_host_name(w_sslhandle.ssl, w_sslhandle.servername)
    result = ropenssl.libssl_SSL_connect(w_sslhandle.ssl)
    if result < 0:
        err = ropenssl.libssl_SSL_get_error(w_sslhandle.ssl, result)
        if err != ropenssl.SSL_ERROR_WANT_READ:
            raise error.PrimitiveFailedError
        copy_bio_ssl(w_sslhandle.writebio, w_dst, n)
        return interp.space.wrap_int(n)
    w_sslhandle.state = SSL_CONNECTED
    # TODO: verify cert
    return interp.space.wrap_int(0)
