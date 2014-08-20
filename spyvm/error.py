
# Some exception classes for the Smalltalk VM

class SmalltalkException(Exception):
    """Base class for Smalltalk exception hierarchy"""
    exception_type = "SmalltalkException"
    _attrs_ = ["msg"]
    def __init__(self, msg="<no message>"):
        self.msg = msg

class PrimitiveFailedError(SmalltalkException):
    exception_type = "PrimitiveFailedError"

class PrimitiveNotYetWrittenError(PrimitiveFailedError):
    exception_type = "PrimitiveNotYetWrittenError"

class UnwrappingError(PrimitiveFailedError):
    exception_type = "UnwrappingError"

class WrappingError(PrimitiveFailedError):
    exception_type = "WrappingError"

class WrapperException(SmalltalkException):
    exception_type = "WrapperException"

class FatalError(SmalltalkException):
    exception_type = "FatalError"

class BlockCannotReturnError(SmalltalkException):
	exception_type = "BlockCannotReturnError"

class MethodNotFound(SmalltalkException):
    exception_type = "MethodNotFound"

class MissingBytecode(SmalltalkException):
    """Bytecode not implemented yet."""
    exception_type = "MissingBytecode"
    def __init__(self, bytecodename):
        SmalltalkException.__init__(self, "Missing bytecode encountered: %s" % bytecodename)

class Exit(Exception):
    _attrs_ = ["msg"]
    def __init__(self, msg):
        self.msg = msg

class CorruptImageError(Exit):
    pass
