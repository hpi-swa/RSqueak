from rsqueakvm import error
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.primitives import index1_0
from rsqueakvm.plugins.plugin import Plugin, PluginStartupScripts

from rpython.rlib import ropenssl
from rpython.rlib.objectmodel import we_are_translated, specialize
from rpython.rtyper.lltypesystem import rffi, lltype


ropenssl.ssl_external("SSL_set_bio",
                      [ropenssl.SSL, ropenssl.BIO, ropenssl.BIO], lltype.Void)
ropenssl.ssl_external("SSL_CTX_use_certificate_file",
                      [ropenssl.SSL_CTX, rffi.CCHARP, rffi.INT], rffi.INT,
                      save_err=rffi.RFFI_FULL_ERRNO_ZERO)
ropenssl.ssl_external("SSL_get_verify_result", [ropenssl.SSL], rffi.INT)
ropenssl.ssl_external("BIO_read",
                      [ropenssl.BIO, rffi.CCHARP, rffi.INT], rffi.INT)
ropenssl.ssl_external("BIO_write",
                      [ropenssl.BIO, rffi.CCHARP, rffi.INT], rffi.INT)
ropenssl.ssl_external("BIO_set_close",
                      [ropenssl.BIO, rffi.LONG], rffi.INT, macro=True)
ropenssl.ssl_external("BIO_ctrl_pending", [ropenssl.BIO], rffi.INT)
ropenssl.ssl_external("X509_NAME_get_text_by_NID",
                      [ropenssl.X509_NAME, rffi.INT, rffi.CCHARP, rffi.INT],
                      rffi.INT)

ropenssl.eci.post_include_bits = tuple(list(ropenssl.eci.post_include_bits) + [
    "#define pypy_ERR_print_errors_stdout() ERR_print_errors_fp(stdout)"
])

ropenssl.ssl_external('pypy_ERR_print_errors_stdout', [], lltype.Void, macro=True)

class CConfig:
    _compilation_info_ = ropenssl.eci
    BIO_CLOSE = ropenssl.rffi_platform.ConstantInteger("BIO_CLOSE")
    BIO_CTRL_PENDING = ropenssl.rffi_platform.ConstantInteger(
        "BIO_CTRL_PENDING")
    X509_V_OK = ropenssl.rffi_platform.ConstantInteger("X509_V_OK")
    NID_commonName = ropenssl.rffi_platform.ConstantInteger("NID_commonName")
for k, v in ropenssl.rffi_platform.configure(CConfig).items():
    globals()[k] = v


def startup(space, argv):
    ropenssl.init_ssl()
    ropenssl.init_digests()
PluginStartupScripts.append(startup)


SqueakSSL = Plugin()
SSL_VERSION = 2

# SSL connection states
SSL_UNUSED = 0
SSL_NOT_CONNECTED = 0
SSL_ACCEPTING = 1
SSL_CONNECTING = 2
SSL_CONNECTED = 3

# Return codes from the core SSL functions
SSL_OK = 0
SSL_NEED_MORE_DATA = -1
SSL_INVALID_STATE = -2
SSL_BUFFER_TOO_SMALL = -3
SSL_INPUT_TOO_LARGE = -4
SSL_GENERIC_ERROR = -5
SSL_OUT_OF_MEMORY = -6

# SqueakSSL certificate status bits.
SSL_NO_CERTIFICATE = -1
SSL_OTHER_ISSUE = 0x0001
SSL_UNTRUSTED_ROOT = 0x0002
SSL_CERT_EXPIRED = 0x0004
SSL_WRONG_USAGE = 0x0008
SSL_INVALID_CN = 0x0010
SSL_CERT_REVOKED = 0x0020

# SqueakSSL getInt/setInt property IDs
PROP_VERSION = 0
PROP_LOGLEVEL = 1
PROP_SSLSTATE = 2
PROP_CERTSTATE = 3

# SqueakSSL getString/setString property IDs
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
        if not we_are_translated():
            self.loglevel = 1
        self.peername = ""
        self.servername = ""
        self.certname = ""
        self.wasclosed = False
        self.readbio = ropenssl.libssl_BIO_new(ropenssl.libssl_BIO_s_mem())
        self.writebio = ropenssl.libssl_BIO_new(ropenssl.libssl_BIO_s_mem())
        ropenssl.libssl_BIO_set_close(self.readbio, BIO_CLOSE)
        ropenssl.libssl_BIO_set_close(self.writebio, BIO_CLOSE)

    @specialize.arg(1)
    @specialize.argtype(2)
    def log(self, str, args=None):
        if self.loglevel:
            if args is not None:
                print str % args
            else:
                print str

    def setup(self):
        _debug_in_interpreter()
        self.log("W_SSLHandle.setup: setting method")
        self.log("W_SSLHandle.setup: Creating context")
        self.ctx = ropenssl.libssl_SSL_CTX_new(ropenssl.libssl_TLS_method())
        # ropenssl.libssl_SSL_CTX_set_verify(
        #     self.ctx, ropenssl.SSL_VERIFY_NONE, None)
        # if ropenssl.libssl_SSL_CTX_set_options(
        #         self.ctx,
        #         ropenssl.SSL_OP_NO_SSLv2 | ropenssl.SSL_OP_NO_SSLv3) <= 0:
        #     return False
        self.log("W_SSLHandle.setup: setting cipher list")
        ropenssl.libssl_SSL_CTX_set_cipher_list(
            self.ctx, "!ADH:HIGH:MEDIUM:@STRENGTH")
        if self.certname:
            self.log("W_SSLHandle.setup: Using cert file %s", self.certname)
            if ropenssl.libssl_SSL_CTX_use_certificate_file(
                    self.ctx, self.certname, ropenssl.SSL_FILETYPE_PEM) <= 0:
                return False
            if ropenssl.libssl_SSL_CTX_use_PrivateKey_file(
                    self.ctx, self.certname, ropenssl.SSL_FILETYPE_PEM) <= 0:
                return False
        self.log("W_SSLHandle.setup: No root CA given; using default verify paths")
        if ropenssl.libssl_SSL_CTX_set_default_verify_paths(self.ctx) <= 0:
            return False
        self.log("W_SSLHandle.setup: Creating SSL")
        self.ssl = ropenssl.libssl_SSL_new(self.ctx)
        self.log("W_SSLHandle.setup: setting bios")
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

def copy_bio_ssl(bio, w_dst, dstlen, loglevel):
    nbytes = ropenssl.libssl_BIO_ctrl_pending(bio)
    if loglevel:
        print ("copy_bio_ssl: %s bytes pending; buffer size %s" %
               (nbytes, dstlen))
    if nbytes > dstlen:  # more bytes to copy than available
        return -1
    with rffi.scoped_alloc_buffer(nbytes) as buf:
        r = ropenssl.libssl_BIO_read(bio, buf.raw, dstlen)
        for idx in range(r):
            w_dst.setchar(idx, buf.raw[idx])
        return r


def _debug_in_interpreter():
    if not we_are_translated():
        import sys
        if "shell" in sys.argv:
            import pdb
            pdb.set_trace()


@SqueakSSL.expose_primitive(unwrap_spec=[object])
def primitiveCreate(interp, s_frame, w_rcvr):
    return W_SSLHandle()

@SqueakSSL.expose_primitive(unwrap_spec=[object, object])
def primitiveDestroy(interp, s_frame, w_rcvr, w_handle):
    _debug_in_interpreter()
    if not isinstance(w_handle, W_SSLHandle):
        return interp.space.wrap_int(0)
    w_handle.close()
    return interp.space.wrap_int(1)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, index1_0, int, object])
def primitiveEncrypt(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):
    _debug_in_interpreter()
    if (not isinstance(w_handle, W_SSLHandle) or
            w_handle.state != SSL_CONNECTED):
        return interp.space.wrap_int(SSL_INVALID_STATE)
    w_handle.log("sqEncryptSSL: Encrypting %s bytes", srclen)
    assert start >= 0 and srclen >= 0
    num_bytes = ropenssl.libssl_SSL_write(w_handle.ssl, src[start:start+srclen], srclen)
    if num_bytes != srclen:
        return interp.space.wrap_int(SSL_GENERIC_ERROR)

    nullp = lltype.nullptr(rffi.VOIDP.TO)
    nbytes = ropenssl.libssl_BIO_ctrl(w_handle.writebio, BIO_CTRL_PENDING, 0, nullp)
    dstlen = w_dst.size()
    if nbytes > dstlen:
        raise error.PrimitiveFailedError

    c = copy_bio_ssl(w_handle.writebio, w_dst, dstlen, w_handle.loglevel)
    return interp.space.wrap_int(c)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, index1_0, int, object])
def primitiveDecrypt(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):
    _debug_in_interpreter()
    if (not isinstance(w_handle, W_SSLHandle) or
            w_handle.state != SSL_CONNECTED):
        return interp.space.wrap_int(SSL_INVALID_STATE)
    assert start >= 0 and srclen >= 0
    nbytes = ropenssl.libssl_BIO_write(w_handle.readbio, src[start:start+srclen], srclen)
    if nbytes != srclen:
        return interp.space.wrap_int(SSL_GENERIC_ERROR)
    dstlen = w_dst.size()
    with rffi.scoped_alloc_buffer(dstlen) as buf:
        nbytes = ropenssl.libssl_SSL_read(w_handle.ssl, buf.raw, dstlen)
        if nbytes <= 0:
            err = ropenssl.libssl_SSL_get_error(w_handle.ssl, nbytes)
            if (err != ropenssl.SSL_ERROR_WANT_READ and
                err != ropenssl.SSL_ERROR_ZERO_RETURN):
                return interp.space.wrap_int(SSL_GENERIC_ERROR)
            else:
                nbytes = 0
        for idx in range(nbytes):
            w_dst.setchar(idx, buf.raw[idx])
    return interp.space.wrap_int(nbytes)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int, str])
def primitiveSetStringProperty(interp, s_frame, w_rcvr, w_handle, propid, value):
    if not isinstance(w_handle, W_SSLHandle):
        return interp.space.wrap_int(0)
    if propid == PROP_CERTNAME:
        w_handle.certname = value
    elif propid == PROP_SERVERNAME:
        w_handle.servername = value
    else:
        w_handle.log("primitiveSetStringProperty: Unknown property ID %s", propid)
        return interp.space.wrap_int(0)
    return interp.space.wrap_int(1)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int])
def primitiveGetStringProperty(interp, s_frame, w_rcvr, w_handle, propid):
    if not isinstance(w_handle, W_SSLHandle):
        return interp.space.w_nil
    if propid == PROP_PEERNAME:
        # r = w_handle.peername
        r = "*"
    elif propid == PROP_CERTNAME:
        r = w_handle.certname
    elif propid == PROP_SERVERNAME:
        r = w_handle.servername
    else:
        w_handle.log("primitiveGetStringProperty: Unknown property ID %s", propid)
        r = ""
    return interp.space.wrap_string(r)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int, int])
def primitiveSetIntProperty(interp, s_frame, w_rcvr, w_handle, propid, value):
    if not isinstance(w_handle, W_SSLHandle):
        return interp.space.wrap_int(0)
    if propid == PROP_LOGLEVEL:
        w_handle.loglevel = value
    else:
        w_handle.log("primitiveSetIntProperty: Unknown property ID %s", propid)
        return interp.space.wrap_int(0)
    return interp.space.wrap_int(1)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, int])
def primitiveGetIntProperty(interp, s_frame, w_rcvr, w_handle, propid):
    if not isinstance(w_handle, W_SSLHandle):
        return interp.space.wrap_int(0)
    if propid == PROP_SSLSTATE:
        r = w_handle.state
    elif propid == PROP_CERTSTATE:
        r = w_handle.certflags
    elif propid == PROP_VERSION:
        r = SSL_VERSION
    else:
        w_handle.log("primitiveGetIntProperty: Unknown property ID %s", propid)
        r = 0
    return interp.space.wrap_int(r)

@SqueakSSL.expose_primitive(unwrap_spec=[object, object, str, index1_0, int, object])
def primitiveConnect(interp, s_frame, w_rcvr, w_handle, src, start, srclen, w_dst):
    _debug_in_interpreter()
    if (not isinstance(w_handle, W_SSLHandle) or
            w_handle.state != SSL_UNUSED and
            w_handle.state != SSL_CONNECTING):
        return interp.space.wrap_int(SSL_INVALID_STATE)
    w_handle.log("primitiveConnect: handle %s", w_handle.hash)
    if w_handle.state == SSL_UNUSED:
        w_handle.state = SSL_CONNECTING
        w_handle.log("primitiveConnect: Setting up SSL")
        if not w_handle.setup():
            return interp.space.wrap_int(SSL_GENERIC_ERROR)
        w_handle.log("primitiveConnect: Setting connect state")
        ropenssl.libssl_SSL_set_connect_state(w_handle.ssl)
    assert start >= 0 and srclen >= 0
    w_handle.log("primitiveConnect: BIO_write %s bytes", srclen)
    n = ropenssl.libssl_BIO_write(w_handle.readbio, src[start:start+srclen], srclen)
    if n < srclen:
        w_handle.log("primitiveConnect: BIO too small for input")
        return interp.space.wrap_int(SSL_GENERIC_ERROR)
    if n < 0:
        w_handle.log("primitiveConnect: BIO_write failed", srclen)
        return interp.space.wrap_int(SSL_GENERIC_ERROR)
    if w_handle.servername:
        w_handle.log("primitiveConnect: Using server name %s", w_handle.servername)
        ropenssl.libssl_SSL_set_tlsext_host_name(w_handle.ssl, w_handle.servername)
    w_handle.log("primitiveConnect: SSL_connect")
    result = ropenssl.libssl_SSL_connect(w_handle.ssl)
    if result <= 0:
        err = ropenssl.libssl_SSL_get_error(w_handle.ssl, result)
        if err != ropenssl.SSL_ERROR_WANT_READ:
            w_handle.log("primitiveConnect: SSL_connect failed with %s", err)
            ropenssl.libssl_pypy_ERR_print_errors_stdout()
            return interp.space.wrap_int(-1)
        w_handle.log("primitiveConnect: copy_bio_ssl")
        c = copy_bio_ssl(w_handle.writebio, w_dst, w_dst.bytesize(),
                         w_handle.loglevel)
        return interp.space.wrap_int(c)
    # We are connected. Verify the cert.
    w_handle.state = SSL_CONNECTED
    w_handle.log("primitiveConnect: SSL_get_peer_certificate")
    cert = ropenssl.libssl_SSL_get_peer_certificate(w_handle.ssl)
    w_handle.log("primitiveConnect: cert = %s", cert)
    if cert:
        x509_name = ropenssl.libssl_X509_get_subject_name(cert)
        with rffi.scoped_alloc_buffer(256) as buf:
            ropenssl.libssl_X509_NAME_get_text_by_NID(
                x509_name, NID_commonName, buf.raw, 256)
            w_handle.peername = rffi.charp2str(buf.raw)
        w_handle.log("primitiveConnect: peerName = %s", w_handle.peername)
        ropenssl.libssl_X509_free(cert)

        # Check the result of verification
        result = ropenssl.libssl_SSL_get_verify_result(w_handle.ssl)
        w_handle.log("primitiveConnect: SSL_get_verify_result = %s (%s)", (result, result == X509_V_OK))
        if result == X509_V_OK:
            w_handle.certflags = SSL_OK
        else:
            w_handle.certflags = SSL_OTHER_ISSUE
    else:
        w_handle.certflags = SSL_NO_CERTIFICATE
    return interp.space.wrap_int(0)
