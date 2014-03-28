"""
Squeak model.

    W_Object
        W_SmallInteger
        W_AbstractObjectWithIdentityHash
            W_Float
            W_AbstractObjectWithClassReference
                W_PointersObject
                W_BytesObject
                W_WordsObject
            W_CompiledMethod

W_BlockContext and W_MethodContext classes have been replaced by functions
that create W_PointersObjects of correct size with attached shadows.
"""
import sys, weakref
from spyvm import constants, error, version, storage_statistics
from spyvm.version import elidable_for_version

from rpython.rlib import rrandom, objectmodel, jit, signature
from rpython.rlib.rarithmetic import intmask, r_uint, r_int
from rpython.rlib.debug import make_sure_not_resized
from rpython.tool.pairtype import extendabletype
from rpython.rlib.objectmodel import instantiate, compute_hash, import_from_mixin, we_are_translated
from rpython.rtyper.lltypesystem import lltype, rffi
from rsdl import RSDL, RSDL_helper

class W_Object(object):
    """Root of Squeak model, abstract."""
    _attrs_ = []    # no RPython-level instance variables allowed in W_Object
    _settled_ = True
    repr_classname = "W_Object"
    bytes_per_slot = constants.BYTES_PER_WORD
    
    def size(self):
        """Return the number of "slots" or "items" in the receiver object.
        This means different things for different objects.
        For ByteObject, this means the number of bytes, for WordObject the number of words,
        for PointerObject the number of pointers (regardless if it's varsized or not).
        """
        return 0
    
    def instsize(self):
        """Return the number of slots of the object reserved for instance variables (not number of bytes).
        Only returns something non-zero for W_PointersObjects,
        because all other classes in this model hierarchy represent varsized classes (except for SmallInteger)."""
        return 0

    def varsize(self):
        """Return number of slots in the of variable-sized part (not number of bytes).
        Not necessarily number of bytes.
        Variable sized objects are those created with #new:."""
        return self.size() - self.instsize()

    def bytesize(self):
        """Return bytesize that conforms to Blue Book.

        The reported size may differ from the actual size in Spy's object
        space, as memory representation varies depending on PyPy translation."""
        return self.size() * self.bytes_per_slot
        
    def getclass(self, space):
        """Return Squeak class."""
        raise NotImplementedError()

    def gethash(self):
        """Return 31-bit hash value."""
        raise NotImplementedError()

    def at0(self, space, index0):
        """Access variable-sized part, as by Object>>at:.

        Return value depends on layout of instance. Byte objects return bytes,
        word objects return words, pointer objects return pointers. Compiled method are
        treated special, if index0 within the literalsize returns pointer to literal,
        otherwise returns byte (ie byte code indexing starts at literalsize)."""
        raise NotImplementedError()

    def atput0(self, space, index0, w_value):
        """Access variable-sized part, as by Object>>at:put:.

        Semantics depend on layout of instance. Byte objects set bytes,
        word objects set words, pointer objects set pointers. Compiled method are
        treated special, if index0 within the literalsize sets pointer to literal,
        otherwise patches bytecode (ie byte code indexing starts at literalsize)."""
        raise NotImplementedError()

    def fetch(self, space, n0):
        """Access fixed-size part, maybe also variable-sized part (we have to
        consult the Blue Book)."""
        # TODO check the Blue Book
        raise NotImplementedError()

    def store(self, space, n0, w_value):
        """Access fixed-size part, maybe also variable-sized part (we have to
        consult the Blue Book)."""
        raise NotImplementedError()

    def fillin(self, space, g_self):
        raise NotImplementedError()

    def getword(self, n0):
        raise NotImplementedError()

    def setword(self, n0, r_uint_value):
        raise NotImplementedError()

    def invariant(self):
        return True

    def getclass(self, space):
        raise NotImplementedError()
        
    def class_shadow(self, space):
        """Return internal representation of Squeak class."""
        return self.getclass(space).as_class_get_shadow(space)

    def is_same_object(self, other):
        """Compare object identity. This should be used instead of directly
        using is everywhere in the interpreter, in case we ever want to
        implement it differently (which is useful e.g. for proxies). Also,
        SmallIntegers and Floats need a different implementation."""
        return self is other

    def become(self, other):
        """Become swaps two objects.
           False means swapping failed"""
        return False

    def clone(self, space):
        raise NotImplementedError

    def has_class(self):
        """All Smalltalk objects should have classes. Unfortuantely for
        bootstrapping the metaclass-cycle and during testing, that is not
        true for some W_PointersObjects"""
        return True

    def classname(self, space):
        """Get the name of the class of the receiver"""
        name = None
        if self.has_class():
            name = self.class_shadow(space).name
        if not name:
            name = "?"
        return name
    
    def lshift(self, space, shift):
        raise error.PrimitiveFailedError()

    def rshift(self, space, shift):
        raise error.PrimitiveFailedError()

    def unwrap_uint(self, space):
        raise error.UnwrappingError("Got unexpected class in unwrap_uint")

    def is_array_object(self):
        return False
        
    # Methods for printing this object
    
    def guess_classname(self):
        """Get the name of the class of the receiver without using a space.
        If the shadow of the class of the receiver is not yet initialized,
        this might not return a correct name."""
        return "?"
    
    def __repr__(self):
        return self.as_repr_string()

    def __str__(self):
        content = self.str_content()
        if content:
            return "a %s(%s)" % (self.guess_classname(), content)
        else:
            return "a %s" % (self.guess_classname())
        
    def str_content(self):
        return ""
    
    def as_repr_string(self):
        return "<%s (a %s) %s>" % (self.repr_classname, self.guess_classname(), self.repr_content())

    def repr_content(self):
        return self.str_content()

class W_SmallInteger(W_Object):
    """Boxed integer value"""
    # TODO can we tell pypy that its never larger then 31-bit?
    _attrs_ = ['value']
    __slots__ = ('value',)     # the only allowed slot here
    _immutable_fields_ = ["value"]
    repr_classname = "W_SmallInteger"
    
    def __init__(self, value):
        self.value = intmask(value)

    def getclass(self, space):
        return space.w_SmallInteger

    def gethash(self):
        return self.value

    def invariant(self):
        return isinstance(self.value, int) and self.value < 0x8000

    def lshift(self, space, shift):
        from rpython.rlib.rarithmetic import ovfcheck, intmask, r_uint
        # shift > 0, therefore the highest bit of upperbound is not set,
        # i.e. upperbound is positive
        upperbound = intmask(r_uint(-1) >> shift)
        if 0 <= self.value <= upperbound:
            shifted = intmask(self.value << shift)
            return space.wrap_positive_32bit_int(shifted)
        else:
            try:
                shifted = ovfcheck(self.value << shift)
            except OverflowError:
                raise error.PrimitiveFailedError()
            return space.wrap_int(shifted)
        raise PrimitiveFailedError

    def rshift(self, space, shift):
        return space.wrap_int(self.value >> shift)

    def unwrap_uint(self, space):
        from rpython.rlib.rarithmetic import r_uint
        val = self.value
        # Assume the caller knows what he does, even if int is negative
        return r_uint(val)

    def guess_classname(self):
        return "SmallInteger"
    
    def str_content(self):
        return "%d" % self.value

    def is_same_object(self, other):
        if not isinstance(other, W_SmallInteger):
            return False
        return self.value == other.value

    def __eq__(self, other):
        if not isinstance(other, W_SmallInteger):
            return False
        return self.value == other.value

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return self.value

    def clone(self, space):
        return self

class W_AbstractObjectWithIdentityHash(W_Object):
    """Object with explicit hash (ie all except small
    ints and floats)."""
    _attrs_ = ['hash']
    repr_classname = "W_AbstractObjectWithIdentityHash"
    
    hash_generator = rrandom.Random()
    UNASSIGNED_HASH = sys.maxint
    hash = UNASSIGNED_HASH # default value

    def fillin(self, space, g_self):
        self.hash = g_self.get_hash()
        
    def setchar(self, n0, character):
        raise NotImplementedError()

    def gethash(self):
        if self.hash == self.UNASSIGNED_HASH:
            self.hash = hash = intmask(self.hash_generator.genrand32()) // 2
            return hash
        return self.hash

    def invariant(self):
        return isinstance(self.hash, int)

    def _become(self, w_other):
        self.hash, w_other.hash = w_other.hash, self.hash

class W_LargePositiveInteger1Word(W_AbstractObjectWithIdentityHash):
    """Large positive integer for exactly 1 word"""
    _attrs_ = ["value", "_exposed_size"]
    repr_classname = "W_LargePositiveInteger1Word"
    bytes_per_slot = 1
    
    def __init__(self, value, size=4):
        self.value = intmask(value)
        self._exposed_size = size

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        word = 0
        bytes = g_self.get_bytes()
        for idx, byte in enumerate(bytes):
            assert idx < 4
            word |= ord(byte) << (idx * 8)
        self.value = intmask(word)
        self._exposed_size = len(bytes)

    def getclass(self, space):
        return space.w_LargePositiveInteger
    
    def guess_classname(self):
        return "LargePositiveInteger"
    
    def invariant(self):
        return isinstance(self.value, int)

    def str_content(self):
        return "%d" % r_uint(self.value)

    def lshift(self, space, shift):
        # shift > 0, therefore the highest bit of upperbound is not set,
        # i.e. upperbound is positive
        upperbound = intmask(r_uint(-1) >> shift)
        if 0 <= self.value <= upperbound:
            shifted = intmask(self.value << shift)
            return space.wrap_positive_32bit_int(shifted)
        else:
            raise error.PrimitiveFailedError()

    def rshift(self, space, shift):
        if shift == 0:
            return self
        # a problem might arrise, because we may shift in ones from left
        mask = intmask((1 << (32 - shift))- 1)
        # the mask is only valid if the highest bit of self.value is set
        # and only in this case we do need such a mask
        return space.wrap_int((self.value >> shift) & mask)

    def unwrap_uint(self, space):
        from rpython.rlib.rarithmetic import r_uint
        return r_uint(self.value)

    def clone(self, space):
        return W_LargePositiveInteger1Word(self.value)

    def at0(self, space, index0):
        if index0 >= self.size():
            raise IndexError()
        shift = index0 * 8
        result = (self.value >> shift) & 0xff
        return space.wrap_int(intmask(result))

    def atput0(self, space, index0, w_byte):
        if index0 >= self.size():
            raise IndexError()
        skew = index0 * 8
        byte = space.unwrap_int(w_byte)
        assert byte <= 0xff
        new_value = self.value & r_uint(~(0xff << skew))
        new_value |= r_uint(byte << skew)
        self.value = intmask(new_value)
    
    def size(self):
        return self._exposed_size
        
    def invariant(self):
        return isinstance(self.value, int)

    def is_array_object(self):
        return True

class W_Float(W_AbstractObjectWithIdentityHash):
    """Boxed float value."""
    _attrs_ = ['value']
    repr_classname = "W_Float"
    
    def fillin_fromwords(self, space, high, low):
        from rpython.rlib.rstruct.ieee import float_unpack
        from rpython.rlib.rarithmetic import r_ulonglong
        r = (r_ulonglong(high) << 32) | low
        self.value = float_unpack(r, 8)

    def __init__(self, value):
        self.value = value

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        high, low = g_self.get_ruints(required_len=2)
        if g_self.reader.version.has_floats_reversed:
            low, high = high, low
        self.fillin_fromwords(space, high, low)

    def getclass(self, space):
        """Return Float from special objects array."""
        return space.w_Float

    def guess_classname(self):
        return "Float"
        
    def str_content(self):
        return "%f" % self.value
        
    def gethash(self):
        return intmask(compute_hash(self.value)) // 2

    def invariant(self):
        return isinstance(self.value, float)

    def _become(self, w_other):
        # TODO -- shouldn't this be named 'become'?
        self.value, w_other.value = w_other.value, self.value
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def is_same_object(self, other):
        if not isinstance(other, W_Float):
            return False
        # TODO is that correct in Squeak?
        return self.value == other.value

    def __eq__(self, other):
        if not isinstance(other, W_Float):
            return False
        return self.value == other.value

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash(self.value)

    def clone(self, space):
        return self

    def at0(self, space, index0):
        return self.fetch(space, index0)

    def atput0(self, space, index0, w_value):
        self.store(space, index0, w_value)

    def fetch(self, space, n0):
        from rpython.rlib.rstruct.ieee import float_pack
        r = float_pack(self.value, 8) # C double
        if n0 == 0:
            return space.wrap_uint(r_uint(intmask(r >> 32)))
        else:
            # bounds-check for primitive access is done in the primitive
            assert n0 == 1
            return space.wrap_uint(r_uint(intmask(r)))

    def store(self, space, n0, w_obj):
        from rpython.rlib.rstruct.ieee import float_unpack, float_pack
        from rpython.rlib.rarithmetic import r_ulonglong

        uint = r_ulonglong(space.unwrap_uint(w_obj))
        r = float_pack(self.value, 8)
        if n0 == 0:
            r = ((r << 32) >> 32) | (uint << 32)
        else:
            assert n0 == 1
            r = ((r >> 32) << 32) | uint
        self.value = float_unpack(r, 8)

    def size(self):
        return constants.WORDS_IN_FLOAT

@signature.finishsigs
class W_AbstractObjectWithClassReference(W_AbstractObjectWithIdentityHash):
    """Objects with arbitrary class (ie not CompiledMethod, SmallInteger or
    Float)."""
    _attrs_ = ['w_class']
    repr_classname = "W_AbstractObjectWithClassReference"
    w_class = None
    
    def __init__(self, space, w_class):
        if w_class is not None:     # it's None only for testing and space generation
            assert isinstance(w_class, W_PointersObject)
            self.w_class = w_class
        else:
            self.w_class = None

    def repr_content(self):
        return 'len=%d %s' % (self.size(), self.str_content())
    
    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        # The class data will be initialized lazily, after the initial fillin-sequence is over.
        # Don't construct the ClassShadow here, yet!
        self.w_class = g_self.get_class()
        
    def getclass(self, space):
        return self.w_class

    def guess_classname(self):
        if self.has_class():
            if self.w_class.has_shadow():
                class_shadow = self.class_shadow(self.w_class.space())
                return class_shadow.name
            else:
                # We cannot access the class during the initialization sequence.
                return "?? (class not initialized)"
        else:
            return "? (no class)"
    
    def invariant(self):
        from spyvm import shadow
        return (W_AbstractObjectWithIdentityHash.invariant(self) and
                isinstance(self.w_class.shadow, shadow.ClassShadow))

    def _become(self, w_other):
        self.w_class, w_other.w_class = w_other.w_class, self.w_class
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def has_class(self):
        return self.w_class is not None

    # we would like the following, but that leads to a recursive import
    #@signature(signature.types.self(), signature.type.any(),
    #           returns=signature.types.instance(ClassShadow))
    def class_shadow(self, space):
        w_class = self.w_class
        assert w_class is not None
        return w_class.as_class_get_shadow(space)

class W_AbstractPointersObject(W_AbstractObjectWithClassReference):
    """Common object."""
    _attrs_ = ['shadow']
    shadow = None
    repr_classname = "W_AbstractPointersObject"
    log_storage = storage_statistics.log
    
    @jit.unroll_safe
    def __init__(self, space, w_class, size):
        """Create new object with size = fixed + variable size."""
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        self.initialize_storage(space, size)
        
    def initialize_storage(self, space, size):
        self.store_shadow(self.empty_storage(space, size))
        self.log_storage("Initialized")
        
    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        # Recursive fillin required to enable specialized storage strategies.
        for g_obj in g_self.pointers:
            g_obj.fillin(space)
        pointers = g_self.get_pointers()
        self.store_shadow(self.storage_for_list(space, pointers))
        self.store_all(space, pointers)
        self.log_storage("Filledin", log_classname=False)
        
    def empty_storage(self, space, size):
        raise NotImplementedError()
    def storage_for_list(self, space, vars):
        raise NotImplementedError()
    
    def assert_shadow(self):
        # Failing the following assert most likely indicates a bug. The shadow can only be absent during
        # the bootstrapping sequence. It will be initialized in the fillin() method. Before that, it should
        # not be switched to a specialized shadow, and the space is also not yet available here! Otherwise,
        # the specialized shadow will attempt to read information from an uninitialized object.
        shadow = self.shadow
        assert shadow, "The shadow has not been initialized yet!"
        return shadow
    
    def switch_shadow(self, new_shadow):
        old_shadow = self.assert_shadow()
        new_shadow.copy_from(old_shadow)
        self.store_shadow(new_shadow)
        new_shadow.attach_shadow()
        self.log_storage("Switched", old_shadow)
    
    def store_with_new_storage(self, new_storage, n0, w_val):
        space = self.space()
        self.switch_shadow(new_storage(space, self, self.size()))
        self.store(space, n0, w_val)
    
    def space(self):
        return self.assert_shadow().space
        
    def __str__(self):
        if self.has_shadow() and self.shadow.provides_getname:
            return self._get_shadow().getname()
        else:
            return W_AbstractObjectWithClassReference.__str__(self)
    
    def repr_content(self):
        shadow_info = "no shadow"
        name = ""
        if self.has_shadow():
            shadow_info = self.shadow.__repr__()
            if self.shadow.provides_getname:
                name = self._get_shadow().getname()
        return '(%s) len=%d [%s]' % (shadow_info, self.size(), name)
        
    def fetch_all(self, space):
        return [self.fetch(space, i) for i in range(self.size())]
    
    def store_all(self, space, collection):
        # Be tolerant: copy over as many elements as possible, set rest to nil.
        # The size of the object cannot be changed in any case.
        my_length = self.size()
        incoming_length = min(my_length, len(collection))
        i = 0
        while i < incoming_length:
            self.store(space, i, collection[i])
            i = i+1
        while i < my_length:
            self.store(space, i, space.w_nil)
            i = i+1
        
    def at0(self, space, index0):
        # To test, at0 = in varsize part
        return self.fetch(space, index0 + self.instsize())

    def atput0(self, space, index0, w_value):
        # To test, at0 = in varsize part
        self.store(space, index0 + self.instsize(), w_value)

    def fetch(self, space, n0):
        return self._get_shadow().fetch(n0)

    def store(self, space, n0, w_value):
        return self._get_shadow().store(n0, w_value)

    def size(self):
        if not self.has_shadow():
            # TODO - this happens only for objects bootstrapped in ObjSpace.
            # Think of a way to avoid this check. Usually, self.shadow is never None.
            return 0
        return self._get_shadow().size()
        
    def instsize(self):
        return self.class_shadow(self.space()).instsize()

    def store_shadow(self, shadow):
        self.shadow = shadow

    def _get_shadow(self):
        return self.shadow
    
    @objectmodel.specialize.arg(2)
    def as_special_get_shadow(self, space, TheClass):
        old_shadow = self._get_shadow()
        shadow = old_shadow
        if not isinstance(old_shadow, TheClass):
            shadow = TheClass(space, self)
            self.switch_shadow(shadow)
        return shadow

    def get_shadow(self, space):
        from spyvm.shadow import AbstractShadow
        return self.as_special_get_shadow(space, AbstractShadow)

    def as_class_get_shadow(self, space):
        from spyvm.shadow import ClassShadow
        return jit.promote(self.as_special_get_shadow(space, ClassShadow))

    def as_blockcontext_get_shadow(self, space):
        from spyvm.shadow import BlockContextShadow
        return self.as_special_get_shadow(space, BlockContextShadow)

    def as_methodcontext_get_shadow(self, space):
        from spyvm.shadow import MethodContextShadow
        return self.as_special_get_shadow(space, MethodContextShadow)

    def as_context_get_shadow(self, space):
        from spyvm.shadow import ContextPartShadow
        # XXX TODO should figure out itself if its method or block context
        if not isinstance(self.shadow, ContextPartShadow):
            if ContextPartShadow.is_block_context(self, space):
                return self.as_blockcontext_get_shadow(space)
            return self.as_methodcontext_get_shadow(space)
        return self.as_special_get_shadow(space, ContextPartShadow)

    def as_methoddict_get_shadow(self, space):
        from spyvm.shadow import MethodDictionaryShadow
        return self.as_special_get_shadow(space, MethodDictionaryShadow)

    def as_cached_object_get_shadow(self, space):
        from spyvm.shadow import CachedObjectShadow
        return self.as_special_get_shadow(space, CachedObjectShadow)

    def as_observed_get_shadow(self, space):
        from spyvm.shadow import ObserveeShadow
        return self.as_special_get_shadow(space, ObserveeShadow)

    def has_shadow(self):
        return self._get_shadow() is not None

    def become(self, w_other):
        if not isinstance(w_other, W_AbstractPointersObject):
            return False
        self.shadow, w_other.shadow = w_other.shadow, self.shadow
        # shadow links are in both directions -> also update shadows
        if    self.shadow is not None:    self.shadow._w_self = self
        if w_other.shadow is not None: w_other.shadow._w_self = w_other
        W_AbstractObjectWithClassReference._become(self, w_other)
        return True

    @jit.unroll_safe
    def clone(self, space):
        my_pointers = self.fetch_all(space)
        w_result = W_PointersObject(space, self.getclass(space), len(my_pointers))
        w_result.store_all(space, my_pointers)
        return w_result
        
class W_PointersObject(W_AbstractPointersObject):
    repr_classname = 'W_PointersObject'
    
    def empty_storage(self, space, size):
        # A newly allocated object contains only nils.
        from spyvm.shadow import AllNilStorageShadow
        return AllNilStorageShadow(space, self, size)
    
    def storage_for_list(self, space, vars):
        #if not self.class_shadow(space).isvariable():
        #   return ListStorageShadow(space, self, len(vars))
        from spyvm.shadow import find_storage_for_objects
        return find_storage_for_objects(space, vars)(space, self, len(vars))

class W_WeakPointersObject(W_AbstractPointersObject):
    repr_classname = 'W_WeakPointersObject'
    
    def empty_storage(self, space, size):
        from spyvm.shadow import WeakListStorageShadow
        return WeakListStorageShadow(space, self, size)
    def storage_for_list(self, space, vars):
        from spyvm.shadow import WeakListStorageShadow
        return WeakListStorageShadow(space, self, len(vars))

class W_BytesObject(W_AbstractObjectWithClassReference):
    _attrs_ = ['bytes', 'c_bytes', '_size']
    _immutable_fields_ = ['_size', 'bytes[*]?']
    repr_classname = 'W_BytesObject'
    bytes_per_slot = 1
    
    def __init__(self, space, w_class, size):
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        assert isinstance(size, int)
        self.bytes = ['\x00'] * size
        self._size = size

    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        self.bytes = g_self.get_bytes()
        self._size = len(self.bytes)

    def at0(self, space, index0):
        return space.wrap_int(ord(self.getchar(index0)))

    def atput0(self, space, index0, w_value):
        self.setchar(index0, chr(space.unwrap_int(w_value)))

    def getchar(self, n0):
        if self.bytes is not None:
            return self.bytes[n0]
        else:
            if n0 >= self._size:
                raise IndexError
            return self.c_bytes[n0]

    def setchar(self, n0, character):
        assert len(character) == 1
        if self.bytes is not None:
            self.bytes[n0] = character
        else:
            self.c_bytes[n0] = character

    def short_at0(self, space, index0):
        byte_index0 = index0 * 2
        byte0 = ord(self.getchar(byte_index0))
        byte1 = ord(self.getchar(byte_index0 + 1)) << 8
        if byte1 & 0x8000 != 0:
            byte1 = intmask(r_uint(0xffff0000) | r_uint(byte1))
        return space.wrap_int(byte1 | byte0)

    def short_atput0(self, space, index0, w_value):
        from rpython.rlib.rarithmetic import int_between
        i_value = space.unwrap_int(w_value)
        if not int_between(-0x8000, i_value, 0x8000):
            raise error.PrimitiveFailedError
        byte_index0 = index0 * 2
        byte0 = i_value & 0xff
        byte1 = (i_value & 0xff00) >> 8
        self.setchar(byte_index0, chr(byte0))
        self.setchar(byte_index0 + 1, chr(byte1))

    def size(self):
        return self._size

    def str_content(self):
        return "'%s'" % self.as_string()

    def as_string(self):
        if self.bytes is not None:
            return "".join(self.bytes)
        else:
            return "".join([self.c_bytes[i] for i in range(self.size())])

    def invariant(self):
        if not W_AbstractObjectWithClassReference.invariant(self):
            return False
        for c in self.bytes:
            if not isinstance(c, str) or len(c) != 1:
                return False
        return True

    def is_same_object(self, other):
        if self is other:
            return True
        # XXX this sounds very wrong to me
        elif not isinstance(other, W_BytesObject):
            return False
        size = self.size()
        if size != other.size():
            return False
        elif size > 256 and self.bytes is not None and other.bytes is not None:
            return self.bytes == other.bytes
        else:
            return self.has_same_chars(other, size)

    @jit.look_inside_iff(lambda self, other, size: size < 256)
    def has_same_chars(self, other, size):
        for i in range(size):
            if self.getchar(i) != other.getchar(i):
                return False
        return True

    def clone(self, space):
        size = self.size()
        w_result = W_BytesObject(space, self.getclass(space), size)
        if self.bytes is not None:
            w_result.bytes = list(self.bytes)
        else:
            w_result.bytes = [self.c_bytes[i] for i in range(size)]
        return w_result

    def unwrap_uint(self, space):
        # TODO: Completely untested! This failed translation bigtime...
        # XXX Probably we want to allow all subclasses
        if not self.getclass(space).is_same_object(space.w_LargePositiveInteger):
            raise error.UnwrappingError("Failed to convert bytes to word")
        word = 0
        for i in range(self.size()):
            word += r_uint(ord(self.getchar(i))) << 8*i
        return word

    def is_array_object(self):
        return True

    def convert_to_c_layout(self):
        if self.bytes is None:
            return self.c_bytes
        else:
            size = self.size()
            c_bytes = self.c_bytes = rffi.str2charp(self.as_string())
            self.bytes = None
            return c_bytes

    def __del__(self):
        if self.bytes is None:
            rffi.free_charp(self.c_bytes)

class W_WordsObject(W_AbstractObjectWithClassReference):
    _attrs_ = ['words', 'c_words', '_size']
    _immutable_fields_ = ['_size']
    repr_classname = "W_WordsObject"
    
    def __init__(self, space, w_class, size):
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        self.words = [r_uint(0)] * size
        self._size = size

    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        self.words = g_self.get_ruints()
        self._size = len(self.words)

    def at0(self, space, index0):
        val = self.getword(index0)
        return space.wrap_uint(val)

    def atput0(self, space, index0, w_value):
        word = space.unwrap_uint(w_value)
        self.setword(index0, word)

    def getword(self, n):
        assert self.size() > n >= 0
        if self.words is not None:
            return self.words[n]
        else:
            return r_uint(self.c_words[n])

    def setword(self, n, word):
        if self.words is not None:
            self.words[n] = r_uint(word)
        else:
            self.c_words[n] = intmask(word)

    def short_at0(self, space, index0):
        word = intmask(self.getword(index0 / 2))
        if index0 % 2 == 0:
            short = word & 0xffff
        else:
            short = (word >> 16) & 0xffff
        if short & 0x8000 != 0:
            short = r_uint(0xffff0000) | r_uint(short)
        return space.wrap_int(intmask(short))

    def short_atput0(self, space, index0, w_value):
        from rpython.rlib.rarithmetic import int_between
        i_value = space.unwrap_int(w_value)
        if not int_between(-0x8000, i_value, 0x8000):
            raise error.PrimitiveFailedError
        word_index0 = index0 / 2
        word = intmask(self.getword(word_index0))
        if index0 % 2 == 0:
            word = intmask(r_uint(word) & r_uint(0xffff0000)) | (i_value & 0xffff)
        else:
            word = (i_value << 16) | (word & 0xffff)
        value = r_uint(word)
        self.setword(word_index0, value)

    def size(self):
        return self._size

    def invariant(self):
        return (W_AbstractObjectWithClassReference.invariant(self) and
                isinstance(self.words, list))

    def clone(self, space):
        size = self.size()
        w_result = W_WordsObject(space, self.getclass(space), size)
        if self.words is not None:
            w_result.words = list(self.words)
        else:
            w_result.words = [r_uint(self.c_words[i]) for i in range(size)]
        return w_result

    def is_array_object(self):
        return True

    def convert_to_c_layout(self):
        if self.words is None:
            return self.c_words
        else:
            from spyvm.interpreter_proxy import sqIntArrayPtr
            size = self.size()
            old_words = self.words
            c_words = self.c_words = lltype.malloc(sqIntArrayPtr.TO, size, flavor='raw')
            for i in range(size):
                c_words[i] = intmask(old_words[i])
            self.words = None
            return c_words

    def as_display_bitmap(self, w_form, interp, sdldisplay=None):
        width = interp.space.unwrap_int(w_form.fetch(interp.space, 1))
        height = interp.space.unwrap_int(w_form.fetch(interp.space, 2))
        depth = interp.space.unwrap_int(w_form.fetch(interp.space, 3))
        if not sdldisplay:
            from spyvm import display
            sdldisplay = display.SDLDisplay(interp.image_name)
            sdldisplay.set_video_mode(width, height, depth)
        w_display_bitmap = W_DisplayBitmap.create(
            interp.space,
            self.getclass(interp.space),
            self.size(),
            depth,
            sdldisplay
        )
        for idx in range(self.size()):
            w_display_bitmap.setword(idx, self.getword(idx))
        w_form.store(interp.space, 0, w_display_bitmap)
        return w_display_bitmap

    def __del__(self):
        if self.words is None:
            lltype.free(self.c_words, flavor='raw')


class W_DisplayBitmap(W_AbstractObjectWithClassReference):
    _attrs_ = ['pixelbuffer', '_realsize', '_real_depth_buffer', 'display', '_depth']
    _immutable_fields_ = ['_realsize', 'display', '_depth']
    repr_classname = "W_DisplayBitmap"
    
    pixelbuffer = None
    
    @staticmethod
    def create(space, w_class, size, depth, display):
        if depth < 8:
            return W_MappingDisplayBitmap(space, w_class, size * (8 / depth), depth, display)
        elif depth == 8:
            return W_8BitDisplayBitmap(space, w_class, size, depth, display)
        elif depth == 16:
            return W_16BitDisplayBitmap(space, w_class, size, depth, display)
        else:
            return W_DisplayBitmap(space, w_class, size, depth, display)

    def repr_content(self):
        return "len=%d depth=%d %s" % (self.size(), self._depth, self.str_content())
    
    def __init__(self, space, w_class, size, depth, display):
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        self._real_depth_buffer = lltype.malloc(rffi.CArray(rffi.UINT), size, flavor='raw')
        self._realsize = size
        self.display = display
        self._depth = depth

    def at0(self, space, index0):
        val = self.getword(index0)
        return space.wrap_uint(val)

    def atput0(self, space, index0, w_value):
        word = space.unwrap_uint(w_value)
        self.setword(index0, word)

    def flush_to_screen(self):
        self.display.flip()

    def size(self):
        return self._realsize
        
    def invariant(self):
        return False

    def clone(self, space):
        w_result = W_WordsObject(space, self.getclass(space), self._realsize)
        n = 0
        while n < self._realsize:
            w_result.words[n] = self.getword(n)
            n += 1
        return w_result

    def getword(self, n):
        assert self.size() > n >= 0
        return self._real_depth_buffer[n]

    def setword(self, n, word):
        self._real_depth_buffer[n] = word
        self.display.get_pixelbuffer()[n] = word

    def is_array_object(self):
        return True

    def update_from_buffer(self):
        for i in range(self._realsize):
            self.setword(i, self.getword(i))

    def convert_to_c_layout(self):
        return self._real_depth_buffer

    def __del__(self):
        lltype.free(self._real_depth_buffer, flavor='raw')


class W_16BitDisplayBitmap(W_DisplayBitmap):
    repr_classname = "W_16BitDisplayBitmap"
    def setword(self, n, word):
        self._real_depth_buffer[n] = word
        mask = 0b11111
        lsb = (r_uint(word) & r_uint(0xffff0000)) >> 16
        msb = (r_uint(word) & r_uint(0x0000ffff))

        lsb = (
            ((lsb >> 10) & mask) |
            (((lsb >> 5) & mask) << 6) |
            ((lsb & mask) << 11)
        )
        msb = (
            ((msb >> 10) & mask) |
            (((msb >> 5) & mask) << 6) |
            ((msb & mask) << 11)
        )

        self.display.get_pixelbuffer()[n] = r_uint(lsb | (msb << 16))


class W_8BitDisplayBitmap(W_DisplayBitmap):
    repr_classname = "W_8BitDisplayBitmap"
    def setword(self, n, word):
        self._real_depth_buffer[n] = word
        self.display.get_pixelbuffer()[n] = r_uint(
            (word >> 24) |
            ((word >> 8) & 0x0000ff00) |
            ((word << 8) & 0x00ff0000) |
            (word << 24)
        )

NATIVE_DEPTH = 8
class W_MappingDisplayBitmap(W_DisplayBitmap):
    repr_classname = "W_MappingDisplayBitmap"
    @jit.unroll_safe
    def setword(self, n, word):
        self._real_depth_buffer[n] = word
        word = r_uint(word)
        pos = self.compute_pos(n)
        assert self._depth <= 4
        rshift = 32 - self._depth
        for i in xrange(8 / self._depth):
            if pos >= self.size():
                return
            mapword = r_uint(0)
            for i in xrange(4):
                pixel = r_uint(word) >> rshift
                mapword |= (r_uint(pixel) << (i * 8))
                word <<= self._depth
            self.display.get_pixelbuffer()[pos] = mapword
            pos += 1

    def compute_pos(self, n):
        return n * (NATIVE_DEPTH / self._depth)

# XXX Shouldn't compiledmethod have class reference for subclassed compiled
# methods?
class W_CompiledMethod(W_AbstractObjectWithIdentityHash):
    """My instances are methods suitable for interpretation by the virtual machine.  This is the only class in the system whose instances intermix both indexable pointer fields and indexable integer fields.

    The current format of a CompiledMethod is as follows:

        header (4 bytes)
        literals (4 bytes each)
        bytecodes  (variable)
    """

    repr_classname = "W_CompiledMethod"
    bytes_per_slot = 1
    _immutable_fields_ = ["_shadow?"]
    _attrs_ = ["bytes", "_likely_methodname", "header", "argsize", "primitive",
                "literals", "tempsize", "literalsize", "islarge", "_shadow"]
### Extension from Squeak 3.9 doc, which we do not implement:
###        trailer (variable)
###    The trailer has two variant formats.  In the first variant, the last
###    byte is at least 252 and the last four bytes represent a source pointer
###    into one of the sources files (see #sourcePointer).  In the second
###    variant, the last byte is less than 252, and the last several bytes
###    are a compressed version of the names of the method's temporary
###    variables.  The number of bytes used for this purpose is the value of
###    the last byte in the method.

    _shadow = None # Default value
    _likely_methodname = "<unknown>"

    def __init__(self, space, bytecount=0, header=0):
        self._shadow = None
        self.setheader(space, header)
        self.bytes = ["\x00"] * bytecount

    def fillin(self, space, g_self):
        # Implicitely sets the header, including self.literalsize
        for i, w_object in enumerate(g_self.get_pointers()):
            self.literalatput0(space, i, w_object)
        self.setbytes(g_self.get_bytes()[(self.literalsize + 1) * 4:])

    def become(self, w_other):
        if not isinstance(w_other, W_CompiledMethod):
            return False
        self.argsize, w_other.argsize = w_other.argsize, self.argsize
        self.primitive, w_other.primitive = w_other.primitive, self.primitive
        self.literals, w_other.literals = w_other.literals, self.literals
        self.tempsize, w_other.tempsize = w_other.tempsize, self.tempsize
        self.bytes, w_other.bytes = w_other.bytes, self.bytes
        self.header, w_other.header = w_other.header, self.header
        self.literalsize, w_other.literalsize = w_other.literalsize, self.literalsize
        self.islarge, w_other.islarge = w_other.islarge, self.islarge
        self._shadow, w_other._shadow = w_other._shadow, self._shadow
        W_AbstractObjectWithIdentityHash._become(self, w_other)
        return True

    def clone(self, space):
        copy = W_CompiledMethod(space, 0, self.getheader())
        copy.bytes = list(self.bytes)
        copy.literals = list(self.literals)
        return copy

    def getclass(self, space):
        return space.w_CompiledMethod

    def guess_classname (self):
        return "CompiledMethod"
        
    def str_content(self):
        return self.get_identifier_string()

    def as_string(self, markBytecode=0):
        from spyvm.interpreter import BYTECODE_TABLE
        j = 1
        retval  = "\nMethodname: " + self.get_identifier_string()
        retval += "\nBytecode:------------\n"
        for i in self.bytes:
            retval += '->' if j is markBytecode else '  '
            retval += ('%0.2i: 0x%0.2x(%0.3i) ' % (j ,ord(i), ord(i))) + BYTECODE_TABLE[ord(i)].__name__ + "\n"
            j += 1
        return retval + "---------------------\n"

    def guess_containing_classname(self):
        from spyvm.shadow import ClassShadow
        guessed_classname = None
        if len(self.literals) > 0:
            w_candidate = self.literals[-1]
            if isinstance(w_candidate, W_PointersObject):
                c_shadow = w_candidate._get_shadow()
                if isinstance(c_shadow, ClassShadow):
                    guessed_classname = c_shadow.getname()
                elif w_candidate.size() >= 2:
                    w_class = w_candidate.fetch(None, 1)
                    if isinstance(w_class, W_PointersObject):
                        d_shadow = w_class._get_shadow()
                        if isinstance(d_shadow, ClassShadow):
                            guessed_classname = d_shadow.getname()
        if guessed_classname:
            class_cutoff = len(guessed_classname) - 6
            if class_cutoff > 0:
                classname = guessed_classname[0:class_cutoff]
            else:
                classname = guessed_classname
        else:
            classname = "<unknown>"
        return classname
    
    def get_identifier_string(self):
        return "%s >> #%s" % (self.guess_containing_classname(), self._likely_methodname)

    def invariant(self):
        return (W_Object.invariant(self) and
                hasattr(self, 'literals') and
                self.literals is not None and
                hasattr(self, 'bytes') and
                self.bytes is not None and
                hasattr(self, 'argsize') and
                self.argsize is not None and
                hasattr(self, 'tempsize') and
                self.tempsize is not None and
                hasattr(self, 'primitive') and
                self.primitive is not None)

    def size(self):
        return self.headersize() + self.getliteralsize() + len(self.bytes)
    
    def gettempsize(self):
        return self.tempsize

    def getliteralsize(self):
        return self.literalsize * constants.BYTES_PER_WORD

    def bytecodeoffset(self):
        return self.getliteralsize() + self.headersize()

    def headersize(self):
        return constants.BYTES_PER_WORD

    def getheader(self):
        return self.header

    def setheader(self, space, header):
        primitive, literalsize, islarge, tempsize, argsize = constants.decode_compiled_method_header(header)
        self.literalsize = literalsize
        self.literals = [space.w_nil] * self.literalsize
        self.header = header
        self.argsize = argsize
        self.tempsize = tempsize
        self.primitive = primitive
        self.islarge = islarge

    def setliterals(self, literals):
        """NOT RPYTHON
           Only for testing"""
        self.literals = literals
        if self.has_shadow():
            self._shadow.update()

    def setbytes(self, bytes):
        self.bytes = bytes

    def as_compiledmethod_get_shadow(self, space):
        from shadow import CompiledMethodShadow
        if self._shadow is None:
            self._shadow = CompiledMethodShadow(self, space)
        return self._shadow

    def literalat0(self, space, index0):
        if index0 == 0:
            return space.wrap_int(self.getheader())
        else:
            return self.literals[index0-1]

    def literalatput0(self, space, index0, w_value):
        if index0 == 0:
            header = space.unwrap_int(w_value)
            self.setheader(space, header)
        else:
            self.literals[index0-1] = w_value
        if self.has_shadow():
            self._shadow.update()

    def store(self, space, index0, w_v):
        self.atput0(space, index0, w_v)

    def at0(self, space, index0):
        if index0 < self.bytecodeoffset():
            # XXX: find out what happens if unaligned
            return self.literalat0(space, index0 / constants.BYTES_PER_WORD)
        else:
            # From blue book:
            # The literal count indicates the size of the
            # CompiledMethod's literal frame.
            # This, in turn, indicates where the
            # CompiledMethod's bytecodes start.
            index0 = index0 - self.bytecodeoffset()
            assert index0 < len(self.bytes)
            return space.wrap_int(ord(self.bytes[index0]))

    def atput0(self, space, index0, w_value):
        if index0 < self.bytecodeoffset():
            if index0 % constants.BYTES_PER_WORD != 0:
                raise error.PrimitiveFailedError("improper store")
            self.literalatput0(space, index0 / constants.BYTES_PER_WORD, w_value)
        else:
            # XXX use to-be-written unwrap_char
            index0 = index0 - self.bytecodeoffset()
            assert index0 < len(self.bytes)
            self.setchar(index0, chr(space.unwrap_int(w_value)))

    def setchar(self, index0, character):
        assert index0 >= 0
        self.bytes[index0] = character
        if self.has_shadow():
            self._shadow.update()

    def has_shadow(self):
        return self._shadow is not None

    def is_array_object(self):
        return True

class DetachingShadowError(Exception):
    def __init__(self, old_shadow, new_shadow_class):
        self.old_shadow = old_shadow
        self.new_shadow_class = new_shadow_class
