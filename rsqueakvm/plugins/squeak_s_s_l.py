from rsqueakvm import error
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.primitives import index1_0
from rsqueakvm.plugins.plugin import Plugin, PluginStartupScripts

from rpython.rlib import ropenssl
from rpython.rlib.objectmodel import we_are_translated
from rpython.rtyper.lltypesystem import rffi, lltype


ropenssl.ssl_external('SSL_set_bio', [ropenssl.SSL, ropenssl.BIO, ropenssl.BIO], lltype.Void)
ropenssl.ssl_external('SSL_CTX_use_certificate_file', [ropenssl.SSL_CTX, rffi.CCHARP, rffi.INT], rffi.INT,
                      save_err=rffi.RFFI_FULL_ERRNO_ZERO)
ropenssl.ssl_external('BIO_read', [ropenssl.BIO, rffi.CCHARP, rffi.INT], rffi.INT)
ropenssl.ssl_external('BIO_write', [ropenssl.BIO, rffi.CCHARP, rffi.INT], rffi.INT)
ropenssl.ssl_external('BIO_set_close', [ropenssl.BIO, rffi.LONG], rffi.INT, macro=True)
ropenssl.ssl_external('BIO_ctrl_pending', [ropenssl.BIO], rffi.INT)


class CConfig:
    _compilation_info_ = ropenssl.eci
    BIO_CLOSE = ropenssl.rffi_platform.ConstantInteger("BIO_CLOSE")
    BIO_CTRL_PENDING = ropenssl.rffi_platform.ConstantInteger("BIO_CTRL_PENDING")
for k, v in ropenssl.rffi_platform.configure(CConfig).items():
    globals()[k] = v


def startup(space, argv):
    ropenssl.init_ssl()
    ropenssl.init_digests()
PluginStartupScripts.append(startup)


SqueakSSL = Plugin()
SSL_VERSION = 2

SSL_UNUSED = 0
SSL_NOT_CONNECTED = 0
SSL_ACCEPTING = 1
SSL_CONNECTING = 2
SSL_CONNECTED = 3

SSL_OK = 0
SSL_NEED_MORE_DATA = -1
SSL_INVALID_STATE = -2
SSL_BUFFER_TOO_SMALL = -3
SSL_INPUT_TOO_LARGE = -4
SSL_GENERIC_ERROR = -5
SSL_OUT_OF_MEMORY = -6

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
               "loglevel", "wasclosed"]
    _immutable_fields_ = ["ctx", "ssl", "readbio", "writebio"]

    def __init__(self):
        self.state = SSL_UNUSED
        self.certflags = 0
        self.loglevel = 0
        self.peername = ""
        self.servername = ""
        self.certname = ""
        self.wasclosed = False
        self.readbio = ropenssl.libssl_BIO_new(ropenssl.libssl_BIO_s_mem())
        self.writebio = ropenssl.libssl_BIO_new(ropenssl.libssl_BIO_s_mem())
        ropenssl.libssl_BIO_set_close(self.readbio, BIO_CLOSE)
        ropenssl.libssl_BIO_set_close(self.writebio, BIO_CLOSE)

    def setup(self):
        _debug_in_interpreter()
        self.ctx = ropenssl.libssl_SSL_CTX_new(ropenssl.libssl_SSLv23_method())
        ropenssl.libssl_SSL_CTX_set_verify(self.ctx, ropenssl.SSL_VERIFY_NONE, lltype.nullptr(rffi.VOIDP.TO))
        # if ropenssl.libssl_SSL_CTX_set_options(
        #         self.ctx, ropenssl.SSL_OP_NO_SSLv2 | ropenssl.SSL_OP_NO_SSLv3) <= 0:
        #     return False
        ropenssl.libssl_SSL_CTX_set_cipher_list(self.ctx, "!ADH:HIGH:MEDIUM:@STRENGTH")
        if self.certname:
            if self.loglevel:
                print "W_SSLHandle.setup: Using cert file %s" % self.certname
            if ropenssl.libssl_SSL_CTX_use_certificate_file(
                    self.ctx, self.certname, ropenssl.SSL_FILETYPE_PEM) <= 0:
                return False
            if ropenssl.libssl_SSL_CTX_use_PrivateKey_file(
                    self.ctx, self.certname, ropenssl.SSL_FILETYPE_PEM) <= 0:
                return False
        if ropenssl.libssl_SSL_CTX_set_default_verify_paths(self.ctx) <= 0:
            return False
        self.ssl = ropenssl.libssl_SSL_new(self.ctx)
        ropenssl.libssl_SSL_set_bio(self.ssl, self.readbio, self.writebio)
        return True

    def getclass(self, space):
        return space.w_SmallInteger

    def close(self):
        self.wasclosed = True
        ropenssl.libssl_BIO_free(self.readbio)
        ropenssl.libssl_BIO_free(self.writebio)
        ropenssl.libssl_SSL_CTX_free(self.ctx)
        # ropenssl.libssl_SSL_free(self.ssl)


def ensured_handle(w_obj):
    if not isinstance(w_obj, W_SSLHandle) or w_obj.wasclosed:
        raise error.PrimitiveFailedError

    return w_obj


def copy_bio_ssl(bio, w_dst, dstlen, loglevel):
    nbytes = ropenssl.libssl_BIO_ctrl_pending(bio)
    if loglevel:
        print ("copy_bio_ssl: %s bytes pending; buffer size %s" %
               (nbytes, dstlen))
    if nbytes > dstlen:  # more bytes to copy than available
        return -1
    with rffi.scoped_alloc_buffer(nbytes) as buf:
        r = ropenssl.libssl_BIO_read(bio, buf.raw, nbytes)
        for idx, char in enumerate(rffi.charp2str(buf.raw)):
            w_dst.setchar(idx, char)
        return r


def _debug_in_interpreter():
    if not we_are_translated():
        import pdb
        pdb.set_trace()


@SqueakSSL.expose_primitive(unwrap_spec=[object])
def primitiveCreate(interp, s_frame, w_rcvr):
    return W_SSLHandle()

@SqueakSSL.expose_primitive(unwrap_spec=[object, object])
def primitiveDestroy(interp, s_frame, w_rcvr, w_sslhandle):
    _debug_in_interpreter()
    ensured_handle(w_sslhandle).close()
    return w_rcvr

# handle, srcBuf, srcLen, dstBuf, dstLen
@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, index1_0, int, object])
def primitiveEncrypt(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):
    _debug_in_interpreter()
    w_sslhandle = ensured_handle(w_handle)
    if w_sslhandle.state != SSL_CONNECTED:
        return interp.space.wrap_int(SSL_INVALID_STATE)
    if w_sslhandle.loglevel:
        print "sqEncryptSSL: Encrypting %s bytes" % srclen
    assert start >= 0 and srclen >= 0
    num_bytes = ropenssl.libssl_SSL_write(w_sslhandle.ssl, src[start:start+srclen], srclen)
    if num_bytes != srclen:
        return interp.space.wrap_int(SSL_GENERIC_ERROR)
    nullp = lltype.nullptr(rffi.VOIDP.TO)
    nbytes = ropenssl.libssl_BIO_ctrl(w_sslhandle.writebio, BIO_CTRL_PENDING, 0, nullp)
    dstlen = w_dst.size()
    if nbytes > dstlen:
        raise error.PrimitiveFailedError
    copy_bio_ssl(w_sslhandle.writebio, w_dst, nbytes, w_sslhandle.loglevel)
    return interp.space.wrap_int(nbytes)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, index1_0, int, object])
def primitiveDecrypt(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):
    _debug_in_interpreter()
    w_sslhandle = ensured_handle(w_handle)
    if w_sslhandle.state != SSL_CONNECTED:
        return interp.space.wrap_int(SSL_INVALID_STATE)
    assert start >= 0 and srclen >= 0
    nbytes = ropenssl.libssl_BIO_write(w_sslhandle.readbio, src[start:start+srclen], srclen)
    if nbytes != srclen:
        return interp.space.wrap_int(SSL_GENERIC_ERROR)
    dstlen = w_dst.size()
    with rffi.scoped_alloc_buffer(dstlen) as buf:
        nbytes = ropenssl.libssl_SSL_read(w_sslhandle.ssl, buf.raw, dstlen)
        if nbytes <= 0:
            err = ropenssl.libssl_SSL_get_error(w_sslhandle.ssl, nbytes)
            if (err != ropenssl.SSL_ERROR_WANT_READ and
                err != ropenssl.SSL_ERROR_ZERO_RETURN):
                return interp.space.wrap_int(SSL_GENERIC_ERROR)
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
        if w_sslhandle.loglevel:
            print 'primitiveSetStringProperty: Unknown property ID %s' % propid
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
        if w_sslhandle.loglevel:
            print 'primitiveGetStringProperty: Unknown property ID %s' % propid
        r = ""
    return interp.space.wrap_string(r)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int, int])
def primitiveSetIntProperty(interp, s_frame, w_rcvr, w_handle, propid, value):
    w_sslhandle = ensured_handle(w_handle)
    if propid == PROP_LOGLEVEL:
        w_sslhandle.loglevel = value
    else:
        if w_sslhandle.loglevel:
            print 'primitiveSetIntProperty: Unknown property ID %s' % propid
        return interp.space.wrap_int(0)
    return interp.space.wrap_int(1)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int])
def primitiveGetIntProperty(interp, s_frame, w_rcvr, w_handle, propid):
    w_sslhandle = ensured_handle(w_handle)
    if propid == PROP_SSLSTATE:
        r = w_sslhandle.state
    elif propid == PROP_CERTSTATE:
        r = w_sslhandle.certflags
    elif propid == PROP_VERSION:
        r = SSL_VERSION
    else:
        if w_sslhandle.loglevel:
            print 'primitiveGetIntProperty: Unknown property ID %s' % propid
        r = 0
    return interp.space.wrap_int(r)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, int, int, object])
def primitiveConnect(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):
    _debug_in_interpreter()
    w_sslhandle = ensured_handle(w_handle)
    if w_sslhandle.loglevel:
        print "primitiveConnect: handle %s" % w_sslhandle.hash
    if w_sslhandle.state != SSL_UNUSED and w_sslhandle.state != SSL_CONNECTING:
        return interp.space.wrap_int(SSL_INVALID_STATE)
    if w_sslhandle.state == SSL_UNUSED:
        w_sslhandle.state = SSL_CONNECTING
        if w_sslhandle.loglevel:
            print "primitiveConnect: Setting up SSL"
        if not w_sslhandle.setup():
            return interp.space.wrap_int(SSL_GENERIC_ERROR)
        if w_sslhandle.loglevel:
            print "primitiveConnect: Setting connect state"
        ropenssl.libssl_SSL_set_connect_state(w_sslhandle.ssl)
    assert start >= 0 and srclen >= 0
    if w_sslhandle.loglevel:
        print "primitiveConnect: BIO_write %s bytes" % srclen
    n = ropenssl.libssl_BIO_write(w_sslhandle.readbio, src[start:start+srclen], srclen)
    if n < srclen:
        if w_sslhandle.loglevel:
            print "primitiveConnect: BIO too small for input"
        return interp.space.wrap_int(SSL_GENERIC_ERROR)
    if n < 0:
        if w_sslhandle.loglevel:
            print "primitiveConnect: BIO_write failed" % srclen
        return interp.space.wrap_int(SSL_GENERIC_ERROR)
    if w_sslhandle.servername:
        if w_sslhandle.loglevel:
            print "primitiveConnect: Using server name %s" % w_sslhandle.servername
        ropenssl.libssl_SSL_set_tlsext_host_name(w_sslhandle.ssl, w_sslhandle.servername)
    result = ropenssl.libssl_SSL_connect(w_sslhandle.ssl)
    if result < 0:
        err = ropenssl.libssl_SSL_get_error(w_sslhandle.ssl, result)
        if err != ropenssl.SSL_ERROR_WANT_READ:
            if w_sslhandle.loglevel:
                print "primitiveConnect: SSL_connect failed"
            return interp.space.wrap_int(-1)
        if w_sslhandle.loglevel:
            print "primitiveConnect: copy_bio_ssl"
        c = copy_bio_ssl(w_sslhandle.writebio, w_dst, w_dst.bytesize(),
                         w_sslhandle.loglevel)
        return interp.space.wrap_int(c)
    w_sslhandle.state = SSL_CONNECTED
    # TODO: verify cert
    return interp.space.wrap_int(0)
