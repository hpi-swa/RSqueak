# struct VirtualMachine* sqGetInterpreterProxy(void);

# typedef struct VirtualMachine {
#   sqInt (*minorVersion)(void);
# } VirtualMachine;

# Loading a Plugin:
#   plugin setInterpreter: proxy.
#   (plugin respondsTo: #initialiseModule) ifTrue:[plugin initialiseModule].
#   plugin perform: primitiveName asSymbol.
from rpython.rlib.entrypoint import entrypoint
from rpython.rtyper.annlowlevel import llhelper
from rpython.rlib.exports import export_struct
from rpython.rtyper.lltypesystem.lltype import FuncType, Ptr
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rlib.unroll import unrolling_iterable

from spyvm import error, model

sqInt = rffi.INT
sqLong = rffi.LONG
sqDouble = rffi.DOUBLE
sqIntArray = rffi.CArray(sqInt)

major = minor = 0
functions = []

def expose_on_virtual_machine_proxy(signature, minor=0, major=1):
    f_ptr = Ptr(signature)
    if minor < minor:
        minor = minor
    if major < major:
        major = major
    def decorator(func):
        functions.append(("c_" + func.func_name, f_ptr, func))
        return func
    return decorator

@expose_on_virtual_machine_proxy(FuncType([], sqInt))
def minorVersion():
    return minor

@expose_on_virtual_machine_proxy(FuncType([], sqInt))
def majorVersion():
    return major

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqInt))
def pop(nItems):
    IProxy.s_frame.pop_n(nItems)
    return 0

@expose_on_virtual_machine_proxy(FuncType([sqInt, sqInt], sqInt))
def popthenPush(nItems, oop):
    s_frame = IProxy.s_frame
    s_frame.pop_n(nItems)
    s_frame.push(IProxy.oop_to_object(oop))
    return 0

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqInt))
def push(oop):
    s_frame = IProxy.s_frame
    s_frame.push(IProxy.oop_to_object(oop))
    return 0

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqInt))
def pushBool(trueOrFalse):
    s_frame = IProxy.s_frame
    if trueOrFalse:
        s_frame.push(IProxy.interp.space.w_true)
    else:
        s_frame.push(IProxy.interp.space.w_false)
    return 0

@expose_on_virtual_machine_proxy(FuncType([sqDouble], sqInt))
def pushFloat(f):
    s_frame = IProxy.s_frame
    s_frame.push(IProxy.space.wrap_float(f))
    return 0

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqInt))
def pushInteger(n):
    s_frame = IProxy.s_frame
    s_frame.push(IProxy.space.wrap_int(n))
    return 0

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqDouble))
def stackFloatValue(offset):
    s_frame = IProxy.s_frame
    f = s_frame.peek(offset)
    if isinstance(f, model.W_Float):
        return f.value
    else:
        IProxy.successFlag = False
        return 0.0

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqInt))
def stackIntegerValue(offset):
    s_frame = IProxy.s_frame
    n = s_frame.peek(offset)
    try:
        return IProxy.space.unwrap_int(n)
    except error.PrimitiveFailedError:
        IProxy.successFlag = False
        return 0

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqInt))
def stackObjectValue(offset):
    s_frame = IProxy.s_frame
    w_object = s_frame.peek(offset)
    if not isinstance(w_object, model.W_SmallInteger):
        return IProxy.object_to_oop(w_object)
    IProxy.successFlag = False
    return 0

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqInt))
def stackValue(offset):
    s_frame = IProxy.s_frame
    return IProxy.object_to_oop(s_frame.peek(offset))

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqInt))
def argumentCountOf(methodOOP):
    w_method = IProxy.oop_to_object(methodOOP)
    if isinstance(w_method, model.W_CompiledMethod):
        return w_method.argsize
    IProxy.successFlag = False
    return 0

@expose_on_virtual_machine_proxy(FuncType([sqInt], Ptr(sqIntArray)))
def arrayValueOf(oop):
    w_array = IProxy.oop_to_object(oop)
    if isinstance(w_array, model.W_WordsObject) or isinstance(w_array, model.W_BytesObject):
        raise NotImplementedError
    IProxy.successFlag = False
    return rffi.cast(Ptr(sqIntArray), 0)

@expose_on_virtual_machine_proxy(FuncType([sqInt], sqInt))
def byteSizeOf(oop):
    w_object = IProxy.oop_to_object(oop)
    s_class = w_object.shadow_of_my_class(IProxy.space)
    size = s_class.instsize()
    if s_class.isvariable():
        size += w_object.primsize(IProxy.space)
    if isinstance(w_object, model.W_BytesObject):
        size *= 4
    return size


# ##############################################################################

VirtualMachine = lltype.Struct("VirtualMachine",
        *map(lambda x: (x[0], x[1]), functions),
        hints={'c_name': 'VirtualMachine'})
VMPtr = Ptr(VirtualMachine)

proxy_functions = unrolling_iterable(functions)

@entrypoint('main', [], c_name='sqGetInterpreterProxy')
def sqGetInterpreterProxy():
    if not IProxy.vm_initialized:
        vm_proxy = lltype.malloc(VirtualMachine, flavor='raw')
        for func_name, signature, func in proxy_functions:
            setattr(vm_proxy, func_name, llhelper(signature, func))
        IProxy.vm_proxy = vm_proxy
        IProxy.vm_initialized = True
    return IProxy.vm_proxy

# rffi.llexternal is supposed to represent c-functions.

class _InterpreterProxy(object):
    _immutable_fields_ = ['vm_initialized?']

    def __init__(self):
        self.vm_proxy = lltype.nullptr(VMPtr.TO)
        self.vm_initialized = False
        self.reset()

    def reset(self):
        self.interp = None
        self.s_frame = None
        self.argcount = 0
        self.s_method = None
        self.successFlag = True

    def call(self, signature, interp, s_frame, argcount, s_method):
        self.interp = interp
        self.s_frame = s_frame
        self.argcount = argcount
        self.s_method = s_method
        self.space = interp.space
        try:
            print "Hello World..."
            raise error.Exit("External Call")
        finally:
            self.reset()

    def oop_to_object(self, oop):
        return self.interp.space.w_nil

    def object_to_oop(self, oop):
        return 0

IProxy = _InterpreterProxy()
