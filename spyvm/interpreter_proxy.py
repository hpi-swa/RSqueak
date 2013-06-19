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
void = lltype.Void

MAJOR = MINOR = 0
functions = []

oop = object()

class ProxyFunctionFailed(error.PrimitiveFailedError):
    pass

def expose_on_virtual_machine_proxy(unwrap_spec, result_type, minor=0, major=1):
    global MINOR, MAJOR
    mapping = {oop: sqInt, int: sqInt, list: sqIntArrayPtr, bool: sqInt,
                float: sqDouble, str: sqStr, long: sqLong, void: void}
    f_ptr = Ptr(FuncType([mapping[spec] for spec in unwrap_spec], mapping[result_type]))
    if MINOR < minor:
        MINOR = minor
    if MAJOR < major:
        MAJOR = major
    def decorator(func):
        len_unwrap_spec = len(unwrap_spec)
        assert (len_unwrap_spec == len(inspect.getargspec(func)[0]) + 1,
                "wrong number of arguments")
        unrolling_unwrap_spec = unrolling_iterable(enumerate(unwrap_spec))
        def wrapped(*c_arguments):
            assert len_unwrap_spec == len(c_arguments)
            args = ()
            print 'Called InterpreterProxy >> %s' % func.func_name,
            try:
                for i, spec in unrolling_unwrap_spec:
                    c_arg = c_arguments[i]
                    if spec is oop:
                        args += (IProxy.oop_to_object(c_arg), )
                    elif spec is str:
                        args += (rffi.charp2str(c_arg), )
                    else:
                        args += (c_arg, )
                result = func(*args)
                print '\t-> %s' % result
                if result_type is oop:
                    assert isinstance(result, model.W_Object)
                    return IProxy.object_to_oop(result)
                elif result_type in (int, float):
                    assert isinstance(result, result_type)
                    return result
                elif result_type is bool:
                    assert isinstance(result, bool)
                    if result:
                        return 1
                    else:
                        return 0
                else:
                    return result
            except error.PrimitiveFailedError:
                print '\t-> failed'
                IProxy.failed()
                if mapping[result_type] is sqInt:
                    return 0
                elif mapping[result_type] is sqDouble:
                    return 0.0
                elif mapping[result_type] is sqIntArrayPtr:
                    return rffi.cast(sqIntArrayPtr, 0)
                elif mapping[result_type] is sqLong:
                    # XXX: how to return a long 0?
                    return 0
                elif mapping[result_type] is sqStr:
                    return rffi.cast(sqStr, "")
                else:
                    raise NotImplementedError(
                        "InterpreterProxy: unknown result_type %s" % (result_type, ))
        wrapped.func_name = "wrapped_ipf_" + func.func_name
        functions.append((func.func_name, f_ptr, wrapped))
        return wrapped
    return decorator

@expose_on_virtual_machine_proxy([], int)
def minorVersion():
    return MINOR

@expose_on_virtual_machine_proxy([], int)
def majorVersion():
    return MAJOR

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
    # if w_array.is_array_object():
    #     return w_array.as_c_array(IProxy)
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], int)
def byteSizeOf(w_object):
    s_class = w_object.shadow_of_my_class(IProxy.space)
    size = s_class.instsize()
    if s_class.isvariable():
        size += w_object.primsize(IProxy.space)
    if not isinstance(w_object, model.W_BytesObject):
        size *= 4
    return size

@expose_on_virtual_machine_proxy([int, oop], list)
def fetchArrayofObject(fieldIndex, w_object):
    # arrayOop := self fetchPointer: fieldIndex ofObject: objectPointer.
    # ^ self arrayValueOf: arrayOop
    w_array = w_object.fetch(IProxy.space, fieldIndex)
    # if w_array.is_array_object():
    #     return w_array.as_c_array(IProxy)
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
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], list)
def firstIndexableField(w_object):
    # return a list with values (?) of w_objects variable-parts
    if isinstance(w_object, model.W_WordsObject):
        return w_object.convert_to_c_layout()
    elif isinstance(w_object, model.W_BytesObject):
        return rffi.cast(sqIntArrayPtr, w_object.convert_to_c_layout())
    else:
        raise ProxyFunctionFailed

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
    return False
#     sqInt (*isKindOf)(sqInt oop, char *aString);

@expose_on_virtual_machine_proxy([oop, str], bool)
def isMemberOf(w_object, name):
    # XXX: stub, until used
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

@expose_on_virtual_machine_proxy([list, list], int)
def becomewith(w_array1, w_array2):
    # XXX: stub, until used
    return 0

@expose_on_virtual_machine_proxy([int], int)
def byteSwapped(w):
    from rpython.rlib.rarithmetic import intmask
    return (w >> 24) & 0xFF + (w >> 8) & 0xFF00 + (w << 8) & 0xFF0000 + (w << 24) & -16777216

@expose_on_virtual_machine_proxy([], bool)
def failed():
    return not IProxy.fail_reason == 0

@expose_on_virtual_machine_proxy([], int)
def fullDisplayUpdate():
    w_display = IProxy.space.objtable['w_display']
    if isinstance(w_display, model.W_DisplayBitmap):
        w_display.flush_to_screen()
        return 0
    else:
        raise ProxyFunctionFailed

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
    IProxy.failed()
    return 0

@expose_on_virtual_machine_proxy([oop, int, int, int, int], int)
def showDisplayBitsLeftTopRightBottom(w_dest_form, l, t, r, b):
    # "Repaint the portion of the Smalltalk screen bounded by the affected
    # rectangle. Used to synchronize the screen after a Bitblt to the Smalltalk
    # Display object."
    # We don't need to copy, because we let the stuf directly write into the
    # display memory
    space = IProxy.space
    if w_dest_form.is_same_object(space.objtable['w_display']):
        w_bitmap = w_dest_form.fetch(space, 0)
        assert isinstance(w_bitmap, model.W_DisplayBitmap)
        w_bitmap.flush_to_screen()
    return 0

@expose_on_virtual_machine_proxy([int], int)
def signalSemaphoreWithIndex(n):
    # ((Smalltalk externalObjects) at: n) signal
    raise ProxyFunctionFailed

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

@expose_on_virtual_machine_proxy([], int)
def compilerHookVector():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], int)
def setCompilerInitialized(n):
    raise ProxyFunctionFailed

#     /* InterpreterProxy methodsFor: 'BitBlt support' */

@expose_on_virtual_machine_proxy([int], int, minor=1)
def loadBitBltFrom(w_bitBlit):
    # bb := bbOop
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], int, minor=1)
def copyBits():
    # bb copyBits
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int, int, int], int, minor=1)
def copyBitsFromtoat(x0, x1, y):
    # bb copyBitsFrom: x0 to: x1 at: y
    raise ProxyFunctionFailed

# #if VM_PROXY_MINOR > 2
@expose_on_virtual_machine_proxy([], oop, minor=2)
def classLargeNegativeInteger():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], oop, minor=2)
def signed32BitIntegerFor(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], int, minor=2)
def signed32BitValueOf(w_number):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop, oop], int, minor=2)
def includesBehaviorThatOf(w_class, w_superclass):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], oop, minor=2)
def primitiveMethod():
    return IProxy.s_method.w_self()

#     /* InterpreterProxy methodsFor: 'FFI support' */

@expose_on_virtual_machine_proxy([], oop, minor=2)
def classExternalAddress():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], oop, minor=2)
def classExternalData():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], oop, minor=2)
def classExternalFunction():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], oop, minor=2)
def classExternalLibrary():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], oop, minor=2)
def classExternalStructure():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int, int], oop, minor=2)
def ioLoadModuleOfLength(modIndex, modLength):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int, int, int], int, minor=2)
def ioLoadSymbolOfLengthFromModule(fnIndex, fnLength, handle):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], bool, minor=2)
def isInMemory(address):
    raise ProxyFunctionFailed
# #endif

# #if VM_PROXY_MINOR > 3
@expose_on_virtual_machine_proxy([str, str], int, minor=3)
def ioLoadFunctionFrom(fnName, modName):
    return rffi.cast(sqInt, IProxy.loadFunctionFrom(modName, fnName))

@expose_on_virtual_machine_proxy([], bool, minor=3)
def ioMicroMSecs():
    raise ProxyFunctionFailed
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

@expose_on_virtual_machine_proxy([long], oop, minor=4)
def positive64BitIntegerFor(integerValue):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], long, minor=4)
def positive64BitValueOf(w_number):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([long], oop, minor=4)
def signed64BitIntegerFor(integerValue):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], long, minor=4)
def signed64BitValueOf(w_number):
    raise ProxyFunctionFailed
# #endif

# #if VM_PROXY_MINOR > 5
@expose_on_virtual_machine_proxy([oop], bool, minor=5)
def isArray(w_object):
    if not isinstance(w_object, model.W_PointersObject):
        return False
    space = IProxy.space
    s_class = w_object.shadow_of_my_class(space)
    if s_class.instsize() == 0 and s_class.isvariable():
        return True
    else:
        return False

@expose_on_virtual_machine_proxy([], int, minor=5)
def forceInterruptCheck():
    raise ProxyFunctionFailed
# #endif

# #if VM_PROXY_MINOR > 6
@expose_on_virtual_machine_proxy([int, oop], oop, minor=6)
def fetchLong32ofObject(fieldFieldIndex, oop):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int, oop], int, minor=6)
def getThisSessionID(fieldFieldIndex, oop):
    # return random int
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([str, str, int, int], oop, minor=6)
def ioFilenamefromStringofLengthresolveAliases(aCharBuffer, filenameIndex, filenameLength, resolveFlag):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], int, minor=6)
def vmEndianness():
    return 0 # return 1 for big endian
# #endif

# #if VM_PROXY_MINOR > 7
#   /* New methods for proxy version 1.8 */

#   /* callbackEnter: Re-enter the interpreter loop for a callback.
#      Arguments:
#        callbackID: Pointer to a location receiving the callback ID
#                    used in callbackLeave
#      Returns: True if successful, false otherwise */
#   sqInt (*callbackEnter)(sqInt *callbackID);
@expose_on_virtual_machine_proxy([int], bool, minor=7)
def callbackEnter(callbackID):
    raise ProxyFunctionFailed

#   /* callbackLeave: Leave the interpreter from a previous callback
#      Arguments:
#        callbackID: The ID of the callback received from callbackEnter()
#      Returns: True if succcessful, false otherwise. */
#   sqInt (*callbackLeave)(sqInt  callbackID);
@expose_on_virtual_machine_proxy([int], bool, minor=7)
def callbackLeave(callbackID):
    raise ProxyFunctionFailed

#   /* addGCRoot: Add a variable location to the garbage collector.
#      The contents of the variable location will be updated accordingly.
#      Arguments:
#        varLoc: Pointer to the variable location
#      Returns: True if successful, false otherwise. */
#   sqInt (*addGCRoot)(sqInt *varLoc);
@expose_on_virtual_machine_proxy([oop], bool, minor=7)
def addGCRoot(callbackID):
    raise ProxyFunctionFailed

#   /* removeGCRoot: Remove a variable location from the garbage collector.
#      Arguments:
#        varLoc: Pointer to the variable location
#      Returns: True if successful, false otherwise.
#   */
#   sqInt (*removeGCRoot)(sqInt *varLoc);
@expose_on_virtual_machine_proxy([oop], bool, minor=7)
def removeGCRoot(callbackID):
    raise ProxyFunctionFailed
# #endif

# #if VM_PROXY_MINOR > 8
@expose_on_virtual_machine_proxy([int], bool, minor=8)
def primitiveFailFor(code):
    if code > 0:
        IProxy.failed(code)
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], void, minor=8)
def setInterruptCheckChain():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], oop, minor=8)
def classAlien():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], oop, minor=8)
def classUnsafeAlien():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int, int, int, int], int, minor=8)
def sendInvokeCallbackStackRegistersJmpbuf(thunkPtrAsInt, stackPtrAsInt, regsPtrAsInt, jmpBufPtrAsInt):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], int, minor=8)
def reestablishContextPriorToCallback(callbackContext):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], int, minor=8)
def getStackPointer():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], bool, minor=8)
def isOopImmutable(w_object):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], bool, minor=8)
def isOopMutable(w_object):
    raise ProxyFunctionFailed
# #endif

# #if VM_PROXY_MINOR > 9
@expose_on_virtual_machine_proxy([int], oop, minor=9)
def methodArg(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], oop, minor=9)
def objectArg(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], int, minor=9)
def integerArg(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], float, minor=9)
def floatArg(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop], int, minor=9)
def methodReturnValue(w_object):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], oop, minor=9)
def topRemappableOop():
    raise ProxyFunctionFailed
# #endif

# #if VM_PROXY_MINOR > 10
# # define DisownVMLockOutFullGC 1
@expose_on_virtual_machine_proxy([int], int, minor=10)
def disownVM(flags):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], int, minor=10)
def ownVM(threadIdAndFlags):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int, int], int, minor=10)
def addHighPriorityTickee(ticker, periodms):
    raise ProxyFunctionFailed
#   void  (*addHighPriorityTickee)(void (*ticker)(void), unsigned periodms);
@expose_on_virtual_machine_proxy([int, int, int], int, minor=10)
def addSynchronousTickee(ticker, periodms, roundms):
    raise ProxyFunctionFailed
#   void  (*addSynchronousTickee)(void (*ticker)(void), unsigned periodms, unsigned roundms);
@expose_on_virtual_machine_proxy([], long, minor=10)
def utcMicroseconds():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], int, minor=10)
def tenuringIncrementalGC():
    raise ProxyFunctionFailed
#   sqInt (*tenuringIncrementalGC)(void);
@expose_on_virtual_machine_proxy([oop], bool, minor=10)
def isYoung(w_object):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([oop, oop], bool, minor=10)
def isKindOfClass(w_object, w_class):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], oop, minor=10)
def primitiveErrorTable():
    raise ProxyFunctionFailed
#   sqInt (*primitiveErrorTable)(void);
@expose_on_virtual_machine_proxy([], int, minor=10)
def primitiveFailureCode():
    return IProxy.fail_reason

@expose_on_virtual_machine_proxy([oop], int, minor=10)
def instanceSizeOf(w_class):
    if isinstance(w_class, model.W_PointersObject):
        s_class = w_class.as_class_get_shadow(IProxy.space)
        return s_class.instsize()
    raise ProxyFunctionFailed
# #endif

# #if VM_PROXY_MINOR > 11
# /* VMCallbackContext opaque type avoids all including setjmp.h & vmCallback.h */
@expose_on_virtual_machine_proxy([int], int, minor=11)
def sendInvokeCallbackContext(vmccp):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int, int, int], int, minor=11)
def returnAsThroughCallbackContext(n, vmccp, m):
    raise ProxyFunctionFailed
#   sqInt (*returnAsThroughCallbackContext)(int, vmccp, sqInt);
@expose_on_virtual_machine_proxy([int], long, minor=11)
def signedMachineIntegerValueOf(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], long, minor=11)
def stackSignedMachineIntegerValue(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], long, minor=11)
def positiveMachineIntegerValueOf(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], long, minor=11)
def stackPositiveMachineIntegerValue(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([], int, minor=11)
def getInterruptPending():
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], str, minor=11)
def cStringOrNullFor(n):
    raise ProxyFunctionFailed

@expose_on_virtual_machine_proxy([int], int, minor=11)
def startOfAlienData(n):
    raise ProxyFunctionFailed
#   void  *(*startOfAlienData)(sqInt);
@expose_on_virtual_machine_proxy([int], int, minor=11)
def sizeOfAlienData(n):
    raise ProxyFunctionFailed
#   usqInt (*sizeOfAlienData)(sqInt);
@expose_on_virtual_machine_proxy([int], int, minor=12)
def signalNoResume(n):
    raise ProxyFunctionFailed
# #endif

# ##############################################################################

VirtualMachine = lltype.Struct("VirtualMachine",
        *map(lambda x: (x[0], x[1]), functions))
VMPtr = Ptr(VirtualMachine)

proxy_functions = unrolling_iterable(functions)

@entrypoint('main', [], c_name='sqGetInterpreterProxy')
def sqGetInterpreterProxy():
    return getInterpreterProxy()

# Redirect of sqGetInterpreterProxy, because the JIT doesn't allow calling of
# functions annotated with @entrypoint.
def getInterpreterProxy():
    if not IProxy.vm_initialized:
        vm_proxy = lltype.malloc(VirtualMachine, flavor='raw')
        for func_name, signature, func in proxy_functions:
            setattr(vm_proxy, func_name, llhelper(signature, func))
        IProxy.vm_proxy = vm_proxy
        IProxy.vm_initialized = True
    return IProxy.vm_proxy

# rffi.llexternal is supposed to represent c-functions.

func_str_void = Ptr(FuncType([], sqStr))
func_bool_vm = Ptr(FuncType([VMPtr], sqInt))
func_bool_void = Ptr(FuncType([], sqInt))

class _InterpreterProxy(object):
    _immutable_fields_ = ['vm_initialized?']

    def __init__(self):
        self.vm_proxy = lltype.nullptr(VMPtr.TO)
        self.vm_initialized = False
        self._next_oop = 0
        self.oop_map = {}
        self.object_map = {}
        self.loaded_modules = {}
        self.remappable_objects = []
        self.reset()

    def reset(self):
        self.interp = None
        self.s_frame = None
        self.argcount = 0
        self.s_method = None
        self.fail_reason = 0

    def call(self, signature, interp, s_frame, argcount, s_method):
        self.initialize_from_call(signature, interp, s_frame, argcount, s_method)
        try:
            # eventual errors are caught by the calling function (EXTERNAL_CALL)
            external_function = rffi.cast(func_bool_void,
                            self.loadFunctionFrom(signature[0], signature[1]))
            print "Calling %s >> %s" % signature
            external_function()

            if not self.fail_reason == 0:
                raise error.PrimitiveFailedError
        finally:
            self.reset()

    def loadFunctionFrom(self, module_name, function_name):
        from rpython.rlib.rdynload import dlsym
        if module_name not in self.loaded_modules:
            module = self.load_and_initialize(module_name)
        else:
            module = self.loaded_modules[module_name]

        try:
            _external_function = dlsym(module, function_name)
        except KeyError:
            raise ProxyFunctionFailed
        else:
            return _external_function


    def initialize_from_call(self, signature, interp, s_frame, argcount, s_method):
        self.interp = interp
        self.s_frame = s_frame
        self.argcount = argcount
        self.s_method = s_method
        self.space = interp.space
        # ensure that space.w_nil gets the first possible oop
        self.object_to_oop(self.space.w_nil)

    def failed(self, reason=1):
        assert reason != 0
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
            print "Mapping new Object: %d -> %s" % (new_index, w_object)
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

    def top_remappable(self):
        if len(self.remappable_objects) == 0:
            raise ProxyFunctionFailed
        return self.remappable_objects[-1]

    def load_and_initialize(self, module_name):
        from rpython.rlib.rdynload import dlopen, dlsym, dlclose, DLOpenError
        import os
        c_name = rffi.str2charp(os.path.join(IProxy.space.executable_path(), module_name))
        try:
            module = dlopen(c_name)
        except DLOpenError, e:
            raise error.PrimitiveFailedError
        try:
            try:
                _getModuleName = dlsym(module, "getModuleName")
            except KeyError:
                pass # the method does not need to exist
            else:
                getModuleName = rffi.cast(func_str_void, _getModuleName)
                if not rffi.charp2str(getModuleName()).startswith(module_name):
                    raise error.PrimitiveFailedError

            try:
                _setInterpreter = dlsym(module, "setInterpreter")
            except KeyError:
                raise error.PrimitiveFailedError
            else:
                setInterpreter = rffi.cast(func_bool_vm, _setInterpreter)
                if not setInterpreter(getInterpreterProxy()):
                    print "Failed setting interpreter on: %s" % module_name
                    raise error.PrimitiveFailedError

            try:
                _initialiseModule = dlsym(module, "initialiseModule")
            except KeyError:
                pass # the method does not need to exist
            else:
                initialiseModule = rffi.cast(func_bool_void, _initialiseModule)
                if not initialiseModule():
                    print "Failed initialization of: %s" % module_name
                    raise error.PrimitiveFailedError

            self.loaded_modules[module_name] = module
            return module
        except error.PrimitiveFailedError:
            dlclose(module)
            raise


IProxy = _InterpreterProxy()

# # Class extensions for Array conversion
# class __extend__(model.W_PointersObject):
#     def as_c_array(self, proxy):
#         return map(lambda x: proxy.object_to_oop(x), self.vars[self.instsize(space):])

# class __extend__(model.W_BytesObject):
#     def as_c_array(self, proxy):
#         print "InterpreterProxy >> as_c_array on BytesObject"
#         raise ProxyFunctionFailed

# class __extend__(model.W_WordsObject):
#     def as_c_array(self, proxy):
#         return map(lambda x: proxy.object_to_oop(proxy.space.wrap_positive_32bit_int(x), self.words)
