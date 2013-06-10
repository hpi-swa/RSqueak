# struct VirtualMachine* sqGetInterpreterProxy(void);

# typedef struct VirtualMachine {
#   sqInt (*minorVersion)(void);
# } VirtualMachine;

# Loading a Plugin:
#   plugin setInterpreter: proxy.
#   (plugin respondsTo: #initialiseModule) ifTrue:[plugin initialiseModule].
#   plugin perform: primitiveName asSymbol.
import inspect

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
sqIntArrayPtr = Ptr(rffi.CArray(sqInt))
sqStr = rffi.CCHARP

major = minor = 0
functions = []

oop = object()

class ProxyFunctionFailed(error.PrimitiveFailedError):
    pass

def expose_on_virtual_machine_proxy(unwrap_spec, result_type, minor=0, major=1):
    mapping = {oop: sqInt, int: sqInt, list: sqIntArrayPtr, bool: sqInt, float: sqDouble, str: sqStr}
    f_ptr = Ptr(FuncType([mapping[spec] for spec in unwrap_spec], mapping[result_type]))
    if minor < minor:
        minor = minor
    if major < major:
        major = major
    def decorator(func):
        len_unwrap_spec = len(unwrap_spec)
        assert (len_unwrap_spec == len(inspect.getargspec(func)[0]) + 1,
                "wrong number of arguments")
        unrolling_unwrap_spec = unrolling_iterable(enumerate(unwrap_spec))
        def wrapped(*c_arguments):
            assert len_unwrap_spec == len(c_arguments)
            args = ()
            try:
                for i, spec in unrolling_unwrap_spec:
                    c_arg = c_arguments[i]
                    if spec is oop:
                        args += (IProxy.oop_to_object(c_arg), )
                    else:
                        args += (c_arg, )
                result = func(*args)
                if result_type is oop:
                    assert isinstance(result, model.W_Object)
                    return IProxy.object_to_oop(result)
                elif result_type is list:
                    assert isinstance(result, list)
                    return IProxy.list_to_carray(result)
                elif result_type in (int, float, bool):
                    assert isinstance(result, result_type)
                    return result
                else:
                    return result
            except error.PrimitiveFailedError:
                IProxy.failed()
                if mapping[result_type] is sqInt:
                    return 0
                elif mapping[result_type] is sqDouble:
                    return 0.0
                elif mapping[result_type] is sqIntArrayPtr:
                    return rffi.cast(sqIntArrayPtr, 0)
                else:
                    raise NotImplementedError(
                        "InterpreterProxy: unknown result_type %s" % (result_type, ))
        wrapped.func_name = "wrapped_ipf_" + func.func_name
        functions.append(("c_" + func.func_name, f_ptr, wrapped))
        return wrapped
    return decorator

@expose_on_virtual_machine_proxy([], int)
def minorVersion():
    return minor

@expose_on_virtual_machine_proxy([], int)
def majorVersion():
    return major

@expose_on_virtual_machine_proxy([int], int)
def pop(nItems):
    IProxy.s_frame.pop_n(nItems)
    return 0

@expose_on_virtual_machine_proxy([int, oop], int)
def popthenPush(nItems, w_object):
    s_frame = IProxy.s_frame
    s_frame.pop_n(nItems)
    s_frame.push(w_object)
    return 0

@expose_on_virtual_machine_proxy([oop], int)
def push(w_object):
    s_frame = IProxy.s_frame
    s_frame.push(w_object)
    return 0

@expose_on_virtual_machine_proxy([bool], int)
def pushBool(trueOrFalse):
    s_frame = IProxy.s_frame
    if trueOrFalse:
        s_frame.push(IProxy.interp.space.w_true)
    else:
        s_frame.push(IProxy.interp.space.w_false)
    return 0

@expose_on_virtual_machine_proxy([float], int)
def pushFloat(f):
    s_frame = IProxy.s_frame
    s_frame.push(IProxy.space.wrap_float(f))
    return 0

@expose_on_virtual_machine_proxy([int], int)
def pushInteger(n):
    s_frame = IProxy.s_frame
    s_frame.push(IProxy.space.wrap_int(n))
    return 0

@expose_on_virtual_machine_proxy([int], float)
def stackFloatValue(offset):
    s_frame = IProxy.s_frame
    f = s_frame.peek(offset)
    return IProxy.space.unwrap_float(f)

@expose_on_virtual_machine_proxy([int], int)
def stackIntegerValue(offset):
    s_frame = IProxy.s_frame
    n = s_frame.peek(offset)
    return IProxy.space.unwrap_int(n)

@expose_on_virtual_machine_proxy([int], oop)
def stackObjectValue(offset):
    s_frame = IProxy.s_frame
    w_object = s_frame.peek(offset)
    if not isinstance(w_object, model.W_SmallInteger):
        return w_object
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], oop)
def stackValue(offset):
    s_frame = IProxy.s_frame
    return s_frame.peek(offset)

@expose_on_virtual_machine_proxy([oop], int)
def argumentCountOf(w_method):
    if isinstance(w_method, model.W_CompiledMethod):
        return w_method.argsize
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], list)
def arrayValueOf(w_array):
    if w_array.is_array_object():
        raise NotImplementedError
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], int)
def byteSizeOf(w_object):
    s_class = w_object.shadow_of_my_class(IProxy.space)
    size = s_class.instsize()
    if s_class.isvariable():
        size += w_object.primsize(IProxy.space)
    if isinstance(w_object, model.W_BytesObject):
        size *= 4
    return size

@expose_on_virtual_machine_proxy([int, oop], list)
def fetchArrayofObject(fieldIndex, w_object):
    # arrayOop := self fetchPointer: fieldIndex ofObject: objectPointer.
    # ^ self arrayValueOf: arrayOop
    w_array = w_object.fetch(IProxy.space, fieldIndex)
    if w_array.is_array_object():
        raise NotImplementedError
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], oop)
def fetchClassOf(w_object):
    w_class = w_object.getclass(IProxy.space)
    return w_class

@expose_on_virtual_machine_proxy([int, oop], float)
def fetchFloatofObject(fieldIndex, w_object):
    space = IProxy.space
    w_float = w_object.fetch(space, fieldIndex)
    return space.unwrap_float(w_float)

@expose_on_virtual_machine_proxy([int, oop], int)
def fetchIntegerofObject(fieldIndex, w_object):
    space = IProxy.space
    w_int = w_object.fetch(space, fieldIndex)
    return space.unwrap_int(w_int)

@expose_on_virtual_machine_proxy([int, oop], oop)
def fetchPointerofObject(fieldIndex, w_object):
    return w_object.fetch(IProxy.space, fieldIndex)

@expose_on_virtual_machine_proxy([int, oop], int)
def obsoleteDontUseThisFetchWordofObject(fieldIndex, w_object):
    # XXX: correctness?
    space = IProxy.space
    w_int = w_object.fetch(space, fieldIndex)
    return space.unwrap_uint(w_int)

@expose_on_virtual_machine_proxy([oop], list)
def firstFixedField(w_object):
    # return a list with oops (?) of w_objects instVars
    raise NotImplementedError

@expose_on_virtual_machine_proxy([oop], list)
def firstIndexableField(w_object):
    # return a list with values (?) of w_objects variable-parts
    raise NotImplementedError

@expose_on_virtual_machine_proxy([int, oop], oop)
def literalofMethod(offset, w_method):
    if isinstance(w_method, model.W_CompiledMethod):
        return w_method.literalat0(IProxy.space, offset)
    else:
        raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], int)
def literalCountOf(w_method):
    if isinstance(w_method, model.W_CompiledMethod):
        return w_method.getliteralsize()
    else:
        raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], int)
def methodArgumentCount():
    return IProxy.argcount

@expose_on_virtual_machine_proxy([], int)
def methodPrimitiveIndex():
    return IProxy.s_method.primitive()

@expose_on_virtual_machine_proxy([oop], int)
def primitiveIndexOf(w_method):
    if isinstance(w_method, model.W_CompiledMethod):
        return w_method.as_compiledmethod_get_shadow().primitive()
    else:
        raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([list], int)
def sizeOfSTArrayFromCPrimitive(c_array):
    raise NotImplementedError

@expose_on_virtual_machine_proxy([oop], int)
def slotSizeOf(w_object):
    return w_object.size()

@expose_on_virtual_machine_proxy([oop, int], oop)
def stObjectat(w_object, n0):
    from spyvm.primitives import assert_valid_index
    space = IProxy.space
    n0 = assert_valid_index(space, n0, w_object)
    return w_object.at0(space, n0)

@expose_on_virtual_machine_proxy([oop, int, oop], oop)
def stObjectatput(w_object, n0, w_value):
    from spyvm.primitives import assert_valid_index
    space = IProxy.space
    n0 = assert_valid_index(space, n0, w_object)
    w_object.atput0(space, n0, w_value)
    return w_value

@expose_on_virtual_machine_proxy([oop], int)
def stSizeOf(w_object):
    return w_object.primsize(IProxy.space)

@expose_on_virtual_machine_proxy([int, oop, int], oop)
def storeIntegerofObjectwithValue(n0, w_object, a):
    if w_object.size() > n0:
        space = IProxy.space
        w_object.store(space, n0, space.wrap_int(a))
        return space.wrap_int(a)
    else:
        raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int, oop, oop], oop)
def storePointerofObjectwithValue(n0, w_object, w_value):
    if w_object.size() > n0:
        w_object.store(IProxy.space, n0, w_value)
        return w_value
    else:
        IProxy.failed()
        return w_value

#     /* InterpreterProxy methodsFor: 'testing' */

@expose_on_virtual_machine_proxy([oop, str], bool)
def isKindOf(w_object, name):
    # XXX: stub, until used
    print "InterpreterProxy >> isKindOf(object, name)"
    return False
#     sqInt (*isKindOf)(sqInt oop, char *aString);

@expose_on_virtual_machine_proxy([oop, str], bool)
def isMemberOf(w_object, name):
    # XXX: stub, until used
    print "InterpreterProxy >> isMemberOf(object, name)"
    return False
#     sqInt (*isMemberOf)(sqInt oop, char *aString);

@expose_on_virtual_machine_proxy([oop], bool)
def isBytes(w_object):
    return isinstance(w_object, model.W_BytesObject)

@expose_on_virtual_machine_proxy([oop], bool)
def isFloatObject(w_object):
    return isinstance(w_object, model.W_Float)

@expose_on_virtual_machine_proxy([oop], bool)
def isIndexable(w_object):
    space = IProxy.space
    return w_object.getclass(space).as_class_get_shadow(space).isvariable()

@expose_on_virtual_machine_proxy([oop], bool)
def isIntegerObject(w_object):
    return isinstance(w_object, model.W_SmallInteger)

@expose_on_virtual_machine_proxy([int], bool)
def isIntegerValue(n):
    """Checking whether the two highest bits are equal,
    which means that the value is representable as 31/63-bit value."""
    return n ^ (n << 1) >= 0

@expose_on_virtual_machine_proxy([oop], bool)
def isPointers(w_object):
    return isinstance(w_object, model.W_PointersObject)

@expose_on_virtual_machine_proxy([oop], bool)
def isWeak(w_object):
    return isinstance(w_object, model.W_WeakPointersObject)

@expose_on_virtual_machine_proxy([oop], bool)
def isWords(w_object):
    return w_object.is_array_object() and not isinstance(w_object, model.W_BytesObject)

@expose_on_virtual_machine_proxy([oop], bool)
def isWordsOrBytes(w_object):
    return w_object.is_array_object()

#     /* InterpreterProxy methodsFor: 'converting' */

@expose_on_virtual_machine_proxy([oop], bool)
def booleanValueOf(w_object):
    space = IProxy.space
    if w_object is space.w_true:
        return True
    if w_object is space.w_false:
        return False
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], int)
def checkedIntegerValueOf(w_object):
    return IProxy.space.unwrap_int(w_object)

@expose_on_virtual_machine_proxy([float], oop)
def floatObjectOf(f):
    return IProxy.space.wrap_float(f)

@expose_on_virtual_machine_proxy([oop], float)
def floatValueOf(w_object):
    return IProxy.space.unwrap_float(w_object)

@expose_on_virtual_machine_proxy([int], oop)
def integerObjectOf(n):
    return IProxy.space.wrap_int(n)

@expose_on_virtual_machine_proxy([oop], int)
def integerValueOf(w_object):
    return IProxy.space.unwrap_int(w_object)

@expose_on_virtual_machine_proxy([int], oop)
def positive32BitIntegerFor(n):
    return IProxy.space.wrap_positive_32bit_int(n)

@expose_on_virtual_machine_proxy([oop], int)
def positive32BitValueOf(n):
    return IProxy.space.unwrap_positive_32bit_int(n)

#     /* InterpreterProxy methodsFor: 'special objects' */

@expose_on_virtual_machine_proxy([], oop)
def characterTable():
    return IProxy.space.w_charactertable

@expose_on_virtual_machine_proxy([], oop)
def displayObject():
    return IProxy.space.objtable['w_display']

@expose_on_virtual_machine_proxy([], oop)
def falseObject():
    return IProxy.space.w_false

@expose_on_virtual_machine_proxy([], oop)
def nilObject():
    return IProxy.space.w_nil

@expose_on_virtual_machine_proxy([], oop)
def trueObject():
    return IProxy.space.w_true

#     /* InterpreterProxy methodsFor: 'special classes' */

# Can't generate these, because the IProxy-field-access can only be done after
# first "call"-call

@expose_on_virtual_machine_proxy([], oop)
def classArray():
    return IProxy.space.w_Array

@expose_on_virtual_machine_proxy([], oop)
def classBitmap():
    return IProxy.space.w_Bitmap

@expose_on_virtual_machine_proxy([], oop)
def classByteArray():
    return IProxy.space.w_ByteArray

@expose_on_virtual_machine_proxy([], oop)
def classCharacter():
    return IProxy.space.w_Character

@expose_on_virtual_machine_proxy([], oop)
def classFloat():
    return IProxy.space.w_Float

@expose_on_virtual_machine_proxy([], oop)
def classLargePositiveInteger():
    return IProxy.space.w_LargePositiveInteger

@expose_on_virtual_machine_proxy([], oop)
def classPoint():
    return IProxy.space.w_Point

@expose_on_virtual_machine_proxy([], oop)
def classSemaphore():
    return IProxy.space.w_Semaphore

@expose_on_virtual_machine_proxy([], oop)
def classSmallInteger():
    return IProxy.space.w_SmallInteger

@expose_on_virtual_machine_proxy([], oop)
def classString():
    return IProxy.space.w_String


#     /* InterpreterProxy methodsFor: 'instance creation' */

@expose_on_virtual_machine_proxy([oop], oop)
def clone(w_object):
    return w_object.clone(IProxy.space)

@expose_on_virtual_machine_proxy([oop, int], oop)
def instantiateClassindexableSize(w_class, varsize):
    if not isinstance(w_class, model.W_PointersObject):
        raise error.PrimitiveFailedError
    s_class = w_class.as_class_get_shadow(IProxy.space)
    return s_class.new(varsize)

@expose_on_virtual_machine_proxy([int, int], oop)
def makePointwithxValueyValue(x, y):
    space = IProxy.space
    w_point = space.w_Point.as_class_get_shadow(space).new()
    w_point.store(space, 0, space.wrap_int(x))
    w_point.store(space, 1, space.wrap_int(y))
    return w_point

@expose_on_virtual_machine_proxy([], oop)
def popRemappableOop():
    return IProxy.pop_remappable()

@expose_on_virtual_machine_proxy([oop], oop)
def pushRemappableOop(w_object):
    return IProxy.push_remappable(w_object)

#     /* InterpreterProxy methodsFor: 'other' */

#     sqInt (*becomewith)(sqInt array1, sqInt array2);
#     sqInt (*byteSwapped)(sqInt w);
#     sqInt (*failed)(void);
#     sqInt (*fullDisplayUpdate)(void);
@expose_on_virtual_machine_proxy([], int)
def fullGC():
    # XXX: how to invoke gc?
    return 0
@expose_on_virtual_machine_proxy([], int)
def incrementalGC():
    # XXX: how to invoke gc?
    return 0

@expose_on_virtual_machine_proxy([], int)
def primitiveFail():
    raise ProxyFunctionFailed

#     sqInt (*showDisplayBitsLeftTopRightBottom)(sqInt aForm, sqInt l, sqInt t, sqInt r, sqInt b);
#     sqInt (*signalSemaphoreWithIndex)(sqInt semaIndex);

@expose_on_virtual_machine_proxy([bool], int)
def success(aBoolean):
    if aBoolean:
        return 0
    else:
        raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], oop)
def superclassOf(w_class):
    if not isinstance(w_class, model.W_PointersObject):
        raise error.PrimitiveFailedError
    s_superclass = w_class.as_class_get_shadow(IProxy.space).s_superclass()
    if s_superclass is not None:
        return s_superclass.w_self()
    else:
        return IProxy.space.w_nil

#     /* InterpreterProxy methodsFor: 'compiler' */

#     CompilerHook *(*compilerHookVector)(void);
#     sqInt          (*setCompilerInitialized)(sqInt initFlag);

# #if VM_PROXY_MINOR > 1

#     /* InterpreterProxy methodsFor: 'BitBlt support' */

#     sqInt (*loadBitBltFrom)(sqInt bbOop);
#     sqInt (*copyBits)(void);
#     sqInt (*copyBitsFromtoat)(sqInt leftX, sqInt rightX, sqInt yValue);

# #endif

# #if VM_PROXY_MINOR > 2

#     sqInt (*classLargeNegativeInteger)(void);
#     sqInt (*signed32BitIntegerFor)(sqInt integerValue);
#     sqInt (*signed32BitValueOf)(sqInt oop);
#     sqInt (*includesBehaviorThatOf)(sqInt aClass, sqInt aSuperClass);
#     sqInt (*primitiveMethod)(void);

#     /* InterpreterProxy methodsFor: 'FFI support' */

#     sqInt (*classExternalAddress)(void);
#     sqInt (*classExternalData)(void);
#     sqInt (*classExternalFunction)(void);
#     sqInt (*classExternalLibrary)(void);
#     sqInt (*classExternalStructure)(void);
#     sqInt (*ioLoadModuleOfLength)(sqInt modIndex, sqInt modLength);
#     sqInt (*ioLoadSymbolOfLengthFromModule)(sqInt fnIndex, sqInt fnLength, sqInt handle);
#     sqInt (*isInMemory)(sqInt address);

# #endif

# #if VM_PROXY_MINOR > 3

#     void *(*ioLoadFunctionFrom)(char *fnName, char *modName);
#     sqInt (*ioMicroMSecs)(void);

# #endif

# #if VM_PROXY_MINOR > 4

# #  if !defined(sqLong)
# #   if _MSC_VER
# #     define sqLong __int64
# #     define usqLong unsigned __int64
# #   else
# #     define sqLong long long
# #     define usqLong unsigned long long
# #   endif
# #  endif

#     sqInt  (*positive64BitIntegerFor)(sqLong integerValue);
#     sqLong (*positive64BitValueOf)(sqInt oop);
#     sqInt  (*signed64BitIntegerFor)(sqLong integerValue);
#     sqLong (*signed64BitValueOf)(sqInt oop);

# #endif

# #if VM_PROXY_MINOR > 5
#     sqInt (*isArray)(sqInt oop);
#     sqInt (*forceInterruptCheck)(void);
# #endif

# #if VM_PROXY_MINOR > 6
#     sqInt  (*fetchLong32ofObject)(sqInt fieldFieldIndex, sqInt oop);
#     sqInt  (*getThisSessionID)(void);
#     sqInt     (*ioFilenamefromStringofLengthresolveAliases)(char* aCharBuffer, char* filenameIndex, sqInt filenameLength, sqInt resolveFlag);
#     sqInt  (*vmEndianness)(void);
# #endif

# #if VM_PROXY_MINOR > 7
#   /* New methods for proxy version 1.8 */

#   /* callbackEnter: Re-enter the interpreter loop for a callback.
#      Arguments:
#        callbackID: Pointer to a location receiving the callback ID
#                    used in callbackLeave
#      Returns: True if successful, false otherwise */
#   sqInt (*callbackEnter)(sqInt *callbackID);

#   /* callbackLeave: Leave the interpreter from a previous callback
#      Arguments:
#        callbackID: The ID of the callback received from callbackEnter()
#      Returns: True if succcessful, false otherwise. */
#   sqInt (*callbackLeave)(sqInt  callbackID);

#   /* addGCRoot: Add a variable location to the garbage collector.
#      The contents of the variable location will be updated accordingly.
#      Arguments:
#        varLoc: Pointer to the variable location
#      Returns: True if successful, false otherwise. */
#   sqInt (*addGCRoot)(sqInt *varLoc);

#   /* removeGCRoot: Remove a variable location from the garbage collector.
#      Arguments:
#        varLoc: Pointer to the variable location
#      Returns: True if successful, false otherwise.
#   */
#   sqInt (*removeGCRoot)(sqInt *varLoc);
# #endif

# #if VM_PROXY_MINOR > 8
#     /* See interp.h and above for standard error codes. */
#     sqInt  (*primitiveFailFor)(sqInt code);
#     void (*(*setInterruptCheckChain)(void (*aFunction)(void)))();
#     sqInt  (*classAlien)(void);
#     sqInt  (*classUnsafeAlien)(void);
#     sqInt  (*sendInvokeCallbackStackRegistersJmpbuf)(sqInt thunkPtrAsInt, sqInt stackPtrAsInt, sqInt regsPtrAsInt, sqInt jmpBufPtrAsInt);
#     sqInt  (*reestablishContextPriorToCallback)(sqInt callbackContext);
#     sqInt *(*getStackPointer)(void);
#     sqInt  (*isOopImmutable)(sqInt oop);
#     sqInt  (*isOopMutable)(sqInt oop);
# #endif

# #if VM_PROXY_MINOR > 9
#   sqInt  (*methodArg)  (sqInt index);
#   sqInt  (*objectArg)  (sqInt index);
#   sqInt  (*integerArg) (sqInt index);
#   double (*floatArg)   (sqInt index);
#   sqInt  (*methodReturnValue) (sqInt oop);
#   sqInt  (*topRemappableOop)  (void);
# #endif

# #if VM_PROXY_MINOR > 10
# # define DisownVMLockOutFullGC 1
#   sqInt (*disownVM)(sqInt flags);
#   sqInt (*ownVM)   (sqInt threadIdAndFlags);
#   void  (*addHighPriorityTickee)(void (*ticker)(void), unsigned periodms);
#   void  (*addSynchronousTickee)(void (*ticker)(void), unsigned periodms, unsigned roundms);
#   usqLong (*utcMicroseconds)(void);
#   sqInt (*tenuringIncrementalGC)(void);
#   sqInt (*isYoung) (sqInt anOop);
#   sqInt (*isKindOfClass)(sqInt oop, sqInt aClass);
#   sqInt (*primitiveErrorTable)(void);
#   sqInt (*primitiveFailureCode)(void);
#   sqInt (*instanceSizeOf)(sqInt aClass);
# #endif

# #if VM_PROXY_MINOR > 11
# /* VMCallbackContext opaque type avoids all including setjmp.h & vmCallback.h */
#   sqInt (*sendInvokeCallbackContext)(vmccp);
#   sqInt (*returnAsThroughCallbackContext)(int, vmccp, sqInt);
#   long  (*signedMachineIntegerValueOf)(sqInt);
#   long  (*stackSignedMachineIntegerValue)(sqInt);
#   unsigned long  (*positiveMachineIntegerValueOf)(sqInt);
#   unsigned long  (*stackPositiveMachineIntegerValue)(sqInt);
#   sqInt  (*getInterruptPending)(void);
#   char  *(*cStringOrNullFor)(sqInt);
#   void  *(*startOfAlienData)(sqInt);
#   usqInt (*sizeOfAlienData)(sqInt);
#   sqInt  (*signalNoResume)(sqInt);
# #endif

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
        self._next_oop = 0
        self.oop_map = {}
        self.object_map = {}
        self.remappable_objects = []
        self.reset()

    def reset(self):
        self.interp = None
        self.s_frame = None
        self.argcount = 0
        self.s_method = None
        self.fail_reason = 0

    def call(self, signature, interp, s_frame, argcount, s_method):
        self.interp = interp
        self.s_frame = s_frame
        self.argcount = argcount
        self.s_method = s_method
        self.space = interp.space
        # ensure that space.w_nil gets the first possible oop
        self.object_to_oop(self.space.w_nil)
        try:
            # Load the correct DLL
            self.failed()
            # call the correct function in it...
            if not self.fail_reason == 0:
                raise error.PrimitiveFailedError
        finally:
            self.reset()

    def failed(self, reason=1):
        self.fail_reason = reason

    def oop_to_object(self, oop):
        try:
            return self.oop_map[oop]
        except KeyError:
            raise ProxyFunctionFailed

    def object_to_oop(self, w_object):
        try:
            return self.object_map[w_object]
        except KeyError:
            new_index = self.next_oop()
            self.oop_map[new_index] = w_object
            self.object_map[w_object] = new_index
            return new_index

    def next_oop(self):
        next_oop = self._next_oop
        self._next_oop = next_oop + 1
        return next_oop

    def pop_remappable(self):
        try:
            return self.remappable_objects.pop()
        except IndexError:
            self.failed()
            return self.space.w_nil

    def push_remappable(self, w_object):
        self.remappable_objects.append(w_object)
        return w_object

IProxy = _InterpreterProxy()
