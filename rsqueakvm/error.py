
# Some exception classes for the Smalltalk VM

class SmalltalkException(Exception):
    """Base class for Smalltalk exception hierarchy"""
    exception_type = "SmalltalkException"
    _attrs_ = ["msg"]
    def __init__(self, msg="<no message>"):
        self.msg = msg

class PrimitiveFailedError(SmalltalkException):
    exception_type = "PrimitiveFailedError"
    def __init__(self, msg="", name=None):
        self.msg = msg
        self.name = name

class SimulatedPrimitiveFailedError(PrimitiveFailedError):
    exception_type = "SimulatedPrimitiveFailedError"
    def __init__(self, msg, w_name, s_class):
        self.msg = msg
        self.w_name = w_name
        self.s_class = s_class

class PrimitiveNotYetWrittenError(PrimitiveFailedError):
    exception_type = "PrimitiveNotYetWrittenError"

class UnwrappingError(PrimitiveFailedError):
    exception_type = "UnwrappingError"

class WrappingError(PrimitiveFailedError):
    exception_type = "WrappingError"

class WrapperException(PrimitiveFailedError):
    exception_type = "WrapperException"

class FatalError(SmalltalkException):
    exception_type = "FatalError"

class BlockCannotReturnError(SmalltalkException):
        exception_type = "BlockCannotReturnError"

class MissingBytecode(SmalltalkException):
    """Bytecode not implemented yet."""
    exception_type = "MissingBytecode"
    def __init__(self, bytecodename):
        SmalltalkException.__init__(
                self, "Missing bytecode encountered: %s" % bytecodename)

class Exit(Exception):
    _attrs_ = ["msg"]
    def __init__(self, msg):
        self.msg = msg

class CorruptImageError(Exit):
    pass

class CleanExit(Exit):
    def __init__(self, msg=""):
        Exit.__init__(self, msg)
