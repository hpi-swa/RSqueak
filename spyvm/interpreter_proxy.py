# struct VirtualMachine* sqGetInterpreterProxy(void);

# typedef struct VirtualMachine {
#   sqInt (*minorVersion)(void);
# } VirtualMachine;

# Loading a Plugin:
#   plugin setInterpreter: proxy.
#   (plugin respondsTo: #initialiseModule) ifTrue:[plugin initialiseModule].
#   plugin perform: primitiveName asSymbol.
from spyvm import error

from rpython.rlib.entrypoint import entrypoint
from rpython.rtyper.annlowlevel import llhelper
from rpython.rlib.exports import export_struct
from rpython.rtyper.lltypesystem.lltype import FuncType, Ptr
from rpython.rtyper.lltypesystem import lltype
from rpython.rlib.unroll import unrolling_iterable

sqInt = lltype.Signed
sqLong = lltype.SignedLongLong

major = minor = 0
functions = []

def expose_on_virtual_machine_proxy(signature, minor=0, major=1):
    f_ptr = Ptr(signature)
    if minor < minor:
        minor = minor
    if major < major:
        major = major
    def decorator(func):
        functions.append((func.func_name, f_ptr, func))
        return func
    return decorator

@expose_on_virtual_machine_proxy(FuncType([], sqInt))
def minorVersion():
    return minor

@expose_on_virtual_machine_proxy(FuncType([], sqInt))
def majorVersion():
    return major

VirtualMachine = lltype.Struct("VirtualMachine",
        *map(lambda x: (x[0], x[1]), functions))
VMPtr = Ptr(VirtualMachine)

proxy_functions = unrolling_iterable(functions)

@entrypoint('main', [], c_name='sqGetInterpreterProxy')
def sqGetInterpreterProxy():
    if not InterpreterProxy.vm_initialized:
        vm_proxy = lltype.malloc(VirtualMachine, flavor='raw')
        for func_name, signature, func in proxy_functions:
            setattr(vm_proxy, func_name, llhelper(signature, func))
        InterpreterProxy.vm_proxy = vm_proxy
        InterpreterProxy.vm_initialized = True
    return InterpreterProxy.vm_proxy

# export_struct("VirtualMachine", VirtualMachine)

class _InterpreterProxy(object):
    _immutable_fields_ = ['vm_initialized?']

    def __init__(self):
        self.vm_proxy = lltype.nullptr(VMPtr.TO)
        self.vm_initialized = False

    def call(self, signature, interp, s_frame, argcount, s_method):
        print "Hello World..."
        raise error.Exit("External Call")

InterpreterProxy = _InterpreterProxy()
