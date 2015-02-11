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
"""
import sys
from spyvm import constants, error
from spyvm.util.version import constant_for_version, constant_for_version_arg, VersionMixin

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

    def is_nil(self, space):
        """Return True, if the receiver represents the nil object in the given Object Space."""
        return self.is_same_object(space.w_nil)

    def is_class(self, space):
        """ Return true, if the receiver seems to be a class.
        We can not be completely sure about this (non-class objects might be
        used as class)."""
        return False

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

    def selector_string(self):
        return self.as_repr_string()

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

    def become(self, w_other):
        if not self.can_become(w_other):
            return False
        if self.is_same_object(w_other):
            return False
        self._become(w_other)
        return True

    def can_become(self, w_other):
        # TODO -- what about become: with a Float and a CompiledMethod etc.?
        # We might be in trouble regarding W_LargePositiveInteger1Word, too.
        return self.__class__ is w_other.__class__

    def _become(self, w_other):
        assert isinstance(w_other, W_AbstractObjectWithIdentityHash)
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

    def _become(self, w_other):
        assert isinstance(w_other, W_LargePositiveInteger1Word)
        self.value, w_other.value = w_other.value, self.value
        self._exposed_size, w_other._exposed_size = w_other._exposed_size, self._exposed_size
        W_AbstractObjectWithIdentityHash._become(self, w_other)

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
        assert isinstance(w_other, W_Float)
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

    def is_class(self, space):
        # This is a class if it's a Metaclass or an instance of a Metaclass.
        if self.has_class():
            w_Metaclass = space.classtable["w_Metaclass"]
            w_class = self.getclass(space)
            if w_Metaclass.is_same_object(w_class):
                return True
            if w_class.has_class():
                return w_Metaclass.is_same_object(w_class.getclass(space))
        return False

    def getclass(self, space):
        return self.w_class

    def guess_classname(self):
        if self.has_class():
            if self.w_class.has_space():
                class_shadow = self.class_shadow(self.w_class.space())
                return class_shadow.name
            else:
                # We cannot access the class during the initialization sequence.
                return "?? (class not initialized)"
        else:
            return "? (no class)"

    def invariant(self):
        from spyvm import storage_classes
        return (W_AbstractObjectWithIdentityHash.invariant(self) and
                isinstance(self.w_class.shadow, storage_classes.ClassShadow))

    def _become(self, w_other):
        assert isinstance(w_other, W_AbstractObjectWithClassReference)
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

class W_PointersObject(W_AbstractObjectWithClassReference):
    """Common object."""
    _attrs_ = ['shadow']
    shadow = None
    repr_classname = "W_PointersObject"

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        """Create new object with size = fixed + variable size."""
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        self.initialize_storage(space, size, weak)

    def initialize_storage(self, space, size, weak=False):
        storage_type = space.strategy_factory.empty_storage_type(self, size, weak)
        space.strategy_factory.set_initial_strategy(self, storage_type, size)

    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        # Recursive fillin required to enable specialized storage strategies.
        for g_obj in g_self.pointers:
            g_obj.fillin(space)
        pointers = g_self.get_pointers()
        storage_type = space.strategy_factory.strategy_type_for(pointers, g_self.isweak())
        space.strategy_factory.set_initial_strategy(self, storage_type, len(pointers), pointers)

    def is_weak(self):
        from storage import WeakListStorageShadow
        return isinstance(self.shadow, WeakListStorageShadow)

    def is_class(self, space):
        from spyvm.storage_classes import ClassShadow
        if isinstance(self.shadow, ClassShadow):
            return True
        return W_AbstractObjectWithClassReference.is_class(self, space)

    def assert_shadow(self):
        # Failing the following assert most likely indicates a bug. The shadow can only be absent during
        # the bootstrapping sequence. It will be initialized in the fillin() method. Before that, it should
        # not be switched to a specialized shadow, and the space is also not yet available here! Otherwise,
        # the specialized shadow will attempt to read information from an uninitialized object.
        shadow = self.shadow
        assert shadow, "The shadow has not been initialized yet!"
        return shadow

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
                name = " [%s]" % self._get_shadow().getname()
        return '(%s) len=%d%s' % (shadow_info, self.size(), name)

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
        old_shadow = self.shadow
        self.shadow = shadow

    def _get_shadow(self):
        return self.shadow

    @objectmodel.specialize.arg(2)
    def as_special_get_shadow(self, space, TheClass):
        old_shadow = self._get_shadow()
        shadow = old_shadow
        if not isinstance(old_shadow, TheClass):
            shadow = space.strategy_factory.switch_strategy(old_shadow, TheClass)
        assert isinstance(shadow, TheClass)
        return shadow

    def get_shadow(self, space):
        from spyvm.storage import AbstractShadow
        return self.as_special_get_shadow(space, AbstractShadow)

    def as_class_get_shadow(self, space):
        from spyvm.storage_classes import ClassShadow
        return jit.promote(self.as_special_get_shadow(space, ClassShadow))

    @objectmodel.specialize.arg(2)
    def as_context_part_get_shadow(self, space, TheClass):
        from spyvm.storage_contexts import ContextPartShadow
        old_shadow = self._get_shadow()
        shadow = old_shadow
        if not isinstance(old_shadow, ContextPartShadow):
            shadow = space.strategy_factory.switch_strategy(old_shadow, TheClass)
        assert isinstance(shadow, ContextPartShadow)
        return shadow

    def as_blockcontext_get_shadow(self, space):
        from spyvm.storage_contexts import BlockContextMarkerClass
        return self.as_context_part_get_shadow(space, BlockContextMarkerClass)

    def as_methodcontext_get_shadow(self, space):
        from spyvm.storage_contexts import MethodContextMarkerClass
        return self.as_context_part_get_shadow(space, MethodContextMarkerClass)

    def as_context_get_shadow(self, space):
        from spyvm.storage_contexts import ContextPartShadow
        shadow = self._get_shadow()
        if not isinstance(shadow, ContextPartShadow):
            if self.getclass(space).is_same_object(space.w_BlockContext):
                return self.as_blockcontext_get_shadow(space)
            if self.getclass(space).is_same_object(space.w_MethodContext):
                return self.as_methodcontext_get_shadow(space)
            raise ValueError("This object cannot be treated like a Context object!")
        else:
            assert isinstance(shadow, ContextPartShadow)
            return shadow

    def as_methoddict_get_shadow(self, space):
        from spyvm.storage_classes import MethodDictionaryShadow
        return self.as_special_get_shadow(space, MethodDictionaryShadow)

    def as_cached_object_get_shadow(self, space):
        from spyvm.storage import CachedObjectShadow
        return self.as_special_get_shadow(space, CachedObjectShadow)

    def as_observed_get_shadow(self, space):
        from spyvm.storage import ObserveeShadow
        return self.as_special_get_shadow(space, ObserveeShadow)

    def has_shadow(self):
        return self._get_shadow() is not None

    def has_space(self):
        # The space is accessed through the shadow.
        return self.has_shadow()

    def _become(self, w_other):
        assert isinstance(w_other, W_PointersObject)

        self.shadow, w_other.shadow = w_other.shadow, self.shadow
        # shadow links are in both directions -> also update shadows
        if self.shadow:
            self.shadow.s_become(self, w_other)
        elif w_other.shadow:
            w_other.shadow.s_become(w_other, self)
        W_AbstractObjectWithClassReference._become(self, w_other)

    @jit.unroll_safe
    def clone(self, space):
        my_pointers = self.fetch_all(space)
        w_result = W_PointersObject(space, self.getclass(space), len(my_pointers))
        w_result.store_all(space, my_pointers)
        return w_result

class W_BytesObject(W_AbstractObjectWithClassReference):
    _attrs_ = ['bytes', 'c_bytes', '_size']
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
        if self.has_class() and self.w_class.has_space():
            if self.w_class.space().omit_printing_raw_bytes.is_set():
                return "<omitted>"
        return "'%s'" % self.as_string().replace('\r', '\n')

    def as_string(self):
        if self.bytes is not None:
            string = "".join(self.bytes)
        else:
            string = "".join([self.c_bytes[i] for i in range(self.size())])
        return string

    def selector_string(self):
        return "#" + self.as_string()

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

    def _become(self, w_other):
        assert isinstance(w_other, W_BytesObject)
        self.bytes, w_other.bytes = w_other.bytes, self.bytes
        self.c_bytes, w_other.c_bytes = w_other.c_bytes, self.c_bytes
        self._size, w_other._size = w_other._size, self._size
        W_AbstractObjectWithClassReference._become(self, w_other)

    def __del__(self):
        if self.bytes is None:
            rffi.free_charp(self.c_bytes)

class W_WordsObject(W_AbstractObjectWithClassReference):
    _attrs_ = ['words', 'c_words', '_size']
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

    def _become(self, w_other):
        assert isinstance(w_other, W_WordsObject)
        self.words, w_other.words = w_other.words, self.words
        self.c_words, w_other.c_words = w_other.c_words, self.c_words
        self._size, w_other._size = w_other._size, self._size
        W_AbstractObjectWithClassReference._become(self, w_other)

    def __del__(self):
        if self.words is None:
            lltype.free(self.c_words, flavor='raw')

class W_CompiledMethod(W_AbstractObjectWithIdentityHash):
    """My instances are methods suitable for interpretation by the virtual machine.  This is the only class in the system whose instances intermix both indexable pointer fields and indexable integer fields.

    The current format of a CompiledMethod is as follows:

        header (4 bytes)
        literals (4 bytes each)
        bytecodes  (variable)

    An optional method trailer can be part of the bytecodes part.
    """

    repr_classname = "W_CompiledMethod"
    bytes_per_slot = 1
    _attrs_ = [ "version",
                # Method header
                "header", "_primitive", "literalsize", "islarge", "_tempsize", "argsize",
                # Main method content
                "bytes", "literals",
                # Additional info about the method
                "lookup_selector", "compiledin_class", "lookup_class" ]

    lookup_selector = "<unknown>"
    lookup_class = None
    import_from_mixin(VersionMixin)

    def __init__(self, space, bytecount=0, header=0):
        self.bytes = ["\x00"] * bytecount
        self.setheader(space, header, initializing=True)

    def fillin(self, space, g_self):
        # Implicitely sets the header, including self.literalsize
        for i, w_object in enumerate(g_self.get_pointers()):
            self.literalatput0(space, i, w_object, initializing=True)
        self.setbytes(g_self.get_bytes()[self.bytecodeoffset():])

    # === Setters ===

    def setheader(self, space, header, initializing=False):
        _primitive, literalsize, islarge, tempsize, argsize = constants.decode_compiled_method_header(header)
        if initializing or self.literalsize != literalsize:
            # Keep the literals if possible.
            self.literalsize = literalsize
            self.literals = [space.w_nil] * self.literalsize
        self.header = header
        self.argsize = argsize
        self._tempsize = tempsize
        self._primitive = _primitive
        self.islarge = islarge
        self.compiledin_class = None
        self.changed()

    def setliteral(self, index, w_lit):
        self.literals[index] = w_lit
        if index == len(self.literals):
            self.compiledin_class = None
        self.changed()

    def setliterals(self, literals):
        """NOT RPYTHON""" # Only for testing, not safe.
        self.literals = literals
        self.compiledin_class = None
        self.changed()

    def set_lookup_class_and_name(self, w_class, selector):
        self.lookup_class = w_class
        self.lookup_selector = selector
        self.changed()

    def setbytes(self, bytes):
        self.bytes = bytes
        self.changed()

    def setchar(self, index0, character):
        assert index0 >= 0
        self.bytes[index0] = character
        self.changed()

    # === Getters ===

    def getclass(self, space):
        return space.w_CompiledMethod

    @constant_for_version
    def size(self):
        return self.headersize() + self.getliteralsize() + len(self.bytes)

    @constant_for_version
    def tempsize(self):
        return self._tempsize

    @constant_for_version
    def getliteralsize(self):
        return self.literalsize * constants.BYTES_PER_WORD

    @constant_for_version
    def bytecodeoffset(self):
        return self.getliteralsize() + self.headersize()

    def headersize(self):
        return constants.BYTES_PER_WORD

    @constant_for_version
    def getheader(self):
        return self.header

    @constant_for_version_arg
    def getliteral(self, index):
        return self.literals[index]

    @constant_for_version
    def primitive(self):
        return self._primitive

    @constant_for_version
    def compute_frame_size(self):
        # From blue book: normal mc have place for 12 temps+maxstack
        # mc for methods with islarge flag turned on 32
        return 16 + self.islarge * 40 + self.argsize

    @constant_for_version_arg
    def fetch_bytecode(self, pc):
        assert pc >= 0 and pc < len(self.bytes)
        return self.bytes[pc]

    def compiled_in(self):
        # This method cannot be constant/elidable. Looking up the compiledin-class from
        # the literals must be done lazily because we cannot analyze the literals
        # properly during the fillin-phase.

        # Prefer the information stored in the CompiledMethod literal...
        result = self.constant_lookup_class()
        if not result:
            # ...but fall back to our own information if nothing else available.
            result = self.constant_compiledin_class()
            if not result:
                self.update_compiledin_class_from_literals()
                result = self.constant_compiledin_class()
        assert result is None or isinstance(result, W_PointersObject)
        return result

    @constant_for_version
    def constant_compiledin_class(self):
        return self.compiledin_class

    @constant_for_version
    def constant_lookup_class(self):
        return self.lookup_class

    def safe_compiled_in(self):
        return self.constant_compiledin_class() or self.constant_lookup_class()

    # === Object Access ===

    def literalat0(self, space, index0):
        if index0 == 0:
            return space.wrap_int(self.getheader())
        else:
            return self.getliteral(index0 - 1)

    def literalatput0(self, space, index0, w_value, initializing=False):
        if index0 == 0:
            header = space.unwrap_int(w_value)
            self.setheader(space, header, initializing=initializing)
        else:
            self.setliteral(index0 - 1, w_value)

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
            index0 = index0 - self.bytecodeoffset()
            assert index0 < len(self.bytes)
            self.setchar(index0, chr(space.unwrap_int(w_value)))

    # === Misc ===

    def update_compiledin_class_from_literals(self):
        # (Blue book, p 607) Last of the literals is either the containing class
        # or an association with compiledin as a class
        literals = self.literals
        if literals and len(literals) > 0:
            w_literal = literals[-1]
            if isinstance(w_literal, W_PointersObject) and w_literal.has_space():
                space = w_literal.space() # Not pretty to steal the space from another object.
                compiledin_class = None
                if w_literal.is_class(space):
                    compiledin_class = w_literal
                elif w_literal.size() >= 2:
                    from spyvm import wrapper
                    association = wrapper.AssociationWrapper(space, w_literal)
                    w_literal = association.value()
                    if w_literal.is_class(space):
                        compiledin_class = w_literal
                if compiledin_class:
                    self.compiledin_class = w_literal
                    self.changed()

    def _become(self, w_other):
        assert isinstance(w_other, W_CompiledMethod)
        self.argsize, w_other.argsize = w_other.argsize, self.argsize
        self._primitive, w_other._primitive = w_other._primitive, self._primitive
        self.literals, w_other.literals = w_other.literals, self.literals
        self._tempsize, w_other._tempsize = w_other._tempsize, self._tempsize
        self.bytes, w_other.bytes = w_other.bytes, self.bytes
        self.header, w_other.header = w_other.header, self.header
        self.literalsize, w_other.literalsize = w_other.literalsize, self.literalsize
        self.islarge, w_other.islarge = w_other.islarge, self.islarge
        self.lookup_selector, w_other.lookup_selector = w_other.lookup_selector, self.lookup_selector
        self.compiledin_class, w_other.compiledin_class = w_other.compiledin_class, self.compiledin_class
        W_AbstractObjectWithIdentityHash._become(self, w_other)
        self.changed()
        w_other.changed()

    def clone(self, space):
        copy = W_CompiledMethod(space, 0, self.getheader())
        copy.bytes = list(self.bytes)
        copy.literals = list(self.literals)
        copy.compiledin_class = self.compiledin_class
        copy.lookup_selector = self.lookup_selector
        copy.changed()
        return copy

    def invariant(self):
        return (W_Object.invariant(self) and
                hasattr(self, 'literals') and
                self.literals is not None and
                hasattr(self, 'bytes') and
                self.bytes is not None and
                hasattr(self, 'argsize') and
                self.argsize is not None and
                hasattr(self, '_tempsize') and
                self._tempsize is not None and
                hasattr(self, '_primitive') and
                self._primitive is not None)

    def is_array_object(self):
        return True

    def create_frame(self, space, receiver, arguments=[]):
        from spyvm.storage_contexts import ContextPartShadow
        assert len(arguments) == self.argsize
        return ContextPartShadow.build_method_context(space, self, receiver, arguments)

    # === Printing ===

    def guess_classname(self):
        return "CompiledMethod"

    def str_content(self):
        return self.get_identifier_string()

    def bytecode_string(self, markBytecode=0):
        from spyvm.interpreter_bytecodes import BYTECODE_TABLE
        retval = "Bytecode:------------"
        j = 1
        for i in self.bytes:
            retval += '\n'
            retval += '->' if j is markBytecode else '  '
            retval += ('%0.2i: 0x%0.2x(%0.3i) ' % (j, ord(i), ord(i))) + BYTECODE_TABLE[ord(i)].__name__
            j += 1
        retval += "\n---------------------"
        return retval

    def as_string(self, markBytecode=0):
        retval  = "\nMethodname: " + self.get_identifier_string()
        retval += "\n%s" % self.bytecode_string(markBytecode)
        return retval

    def guess_containing_classname(self):
        w_class = self.compiled_in()
        if w_class and w_class.has_space():
            # Not pretty to steal the space from another object.
            return w_class.as_class_get_shadow(w_class.space()).getname()
        return "? (no compiledin-info)"

    def get_identifier_string(self):
        return "%s >> #%s" % (self.guess_containing_classname(), self.lookup_selector)

    def safe_identifier_string(self):
        if not we_are_translated():
            return self.get_identifier_string()
        # This has the same functionality as get_identifier_string, but without calling any
        # methods in order to avoid side effects that prevent translation.
        w_class = self.safe_compiled_in()
        if isinstance(w_class, W_PointersObject):
            from spyvm.storage_classes import ClassShadow
            s_class = w_class.shadow
            if isinstance(s_class, ClassShadow):
                return "%s >> #%s" % (s_class.getname(), self.lookup_selector)
        return "#%s" % self.lookup_selector
