"""
Squeak model.

    W_Object
        W_SmallInteger
        W_AbstractObjectWithIdentityHash
            W_LargePositiveInteger1Word
            W_Float
            W_Character
            W_PointersObject
            W_AbstractObjectWithClassReference
                W_BytesObject
                W_WordsObject
            W_CompiledMethod
                W_SpurCompiledMethod
                W_PreSpurCompiledMethod
"""
import sys, math
from spyvm import constants, error
from spyvm.util.version import constant_for_version, constant_for_version_arg, VersionMixin, Version

from rpython.rlib import rrandom, objectmodel, jit, signature, longlong2float
from rpython.rlib.rarithmetic import intmask, r_uint, r_int, ovfcheck, r_longlong
from rpython.rlib.debug import make_sure_not_resized
from rpython.tool.pairtype import extendabletype
from rpython.rlib.objectmodel import instantiate, compute_hash, import_from_mixin, we_are_translated
from rpython.rtyper.lltypesystem import lltype, rffi
from rsdl import RSDL, RSDL_helper
from rpython.rlib.rstrategies import rstrategies as rstrat


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

    def change_class(self, space, w_class):
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

    def fillin_weak(self, space, g_self):
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
        w_class = jit.promote(self.getclass(space))
        return w_class.as_class_get_shadow(space)

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

    def pointers_become_one_way(self, space, from_w, to_w):
        pass

    def post_become_one_way(self, w_to):
        pass

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

    def unwrap_int(self, space):
        raise error.UnwrappingError("Got unexpected class in unwrap_int")

    def unwrap_uint(self, space):
        raise error.UnwrappingError("Got unexpected class in unwrap_uint")

    def unwrap_positive_32bit_int(self, space):
        raise error.UnwrappingError("Got unexpected class unwrap_positive_32bit_int")

    def unwrap_longlong(self, space):
        raise error.UnwrappingError("Got unexpected class unwrap_longlong")

    def unwrap_long_untranslated(self, space):
        return self.unwrap_longlong(space)


    def unwrap_char_as_byte(self, space):
        raise error.UnwrappingError

    def unwrap_array(self, space):
        raise error.UnwrappingError

    def unwrap_float(self, space):
        raise error.UnwrappingError

    def is_array_object(self):
        return False

    def unwrap_string(self, space):
        raise error.UnwrappingError

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

    def unwrap_int(self, space):
        return intmask(self.value)

    def unwrap_uint(self, space):
        val = self.value
        # Assume the caller knows what he does, even if int is negative
        return r_uint(val)

    def unwrap_positive_32bit_int(self, space):
        if self.value >= 0:
            return r_uint(self.value)
        else:
            raise error.UnwrappingError

    def unwrap_longlong(self, space):
        return r_longlong(self.value)

    def unwrap_float(self, space):
        return float(self.value)

    def unwrap_char_as_byte(self, space):
        # We do not implement the STRING_REPLACE primitive, but some code paths
        # in Squeak rely on that primitive's munging of ByteArrays and
        # ByteStrings. We are forgiving, so we also allow bytes extraced from
        # ByteArrays to be unwrapped as characters and put into strings
        from rpython.rlib.rarithmetic import int_between
        value = self.value
        if not int_between(0, value, 255):
            raise error.UnwrappingError
        else:
            return chr(self.value)

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
    _immutable_fields_ = ['hash?']
    repr_classname = "W_AbstractObjectWithIdentityHash"

    hash_generator = rrandom.Random()
    UNASSIGNED_HASH = sys.maxint
    hash = UNASSIGNED_HASH # default value

    def post_become_one_way(self, w_to):
        if isinstance(w_to, W_AbstractObjectWithIdentityHash):
            w_to.hash = self.gethash()

    def fillin(self, space, g_self):
        self.hash = g_self.get_hash()

    def setchar(self, n0, character):
        raise NotImplementedError()

    def gethash(self):
        if self.hash == self.UNASSIGNED_HASH:
            self.hash = hash = intmask(self.hash_generator.genrand32()) % 2**22
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

    def has_class(self):
        return True

    def getclass(self, space):
        return space.w_LargePositiveInteger

    def guess_classname(self):
        return "LargePositiveInteger"

    def invariant(self):
        return isinstance(self.value, int)

    def str_content(self):
        return "%d" % r_uint(self.value)

    def unwrap_string(self, space):
        # OH GOD! TODO: Make this sane!
        res = [chr(self.value & r_uint(0x000000ff)), chr((self.value & r_uint(0x0000ff00)) >> 8), chr((self.value & r_uint(0x00ff0000)) >> 16), chr((self.value & r_uint(0xff000000)) >> 24)]
        return "".join(res)

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

    def unwrap_int(self, space):
        if self.value >= 0:
            return intmask(self.value)
        else:
            raise error.UnwrappingError("The value is negative when interpreted as 32bit value.")

    def unwrap_uint(self, space):
        return r_uint(self.value)

    def unwrap_positive_32bit_int(self, space):
        return r_uint(self.value)

    def unwrap_longlong(self, space):
        return r_longlong(r_uint(self.value))

    def unwrap_float(self, space):
        return float(self.value)

    def clone(self, space):
        return W_LargePositiveInteger1Word(self.value)

    def at0(self, space, index0):
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

    def unwrap_string(self, space):
        word = longlong2float.float2longlong(self.value)
        return "".join([chr(word & 0x000000ff),
                        chr((word >> 8) & 0x000000ff),
                        chr((word >> 16) & 0x000000ff),
                        chr((word >> 24) & 0x000000ff),
                        chr((word >> 32) & 0x000000ff),
                        chr((word >> 40) & 0x000000ff),
                        chr((word >> 48) & 0x000000ff),
                        chr((word >> 56) & 0x000000ff)])

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        high, low = g_self.get_ruints(required_len=2)
        if g_self.reader.version.has_floats_reversed:
            low, high = high, low
        self.fillin_fromwords(space, high, low)

    def has_class(self):
        return True

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
        return self.value == other.value or (math.isnan(self.value) and math.isnan(other.value))

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

    def unwrap_float(self, space):
        return self.value

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

class W_Character(W_AbstractObjectWithIdentityHash):
    """Boxed char value."""
    _attrs_ = ['value']
    repr_classname = "W_Character"

    def __init__(self, value):
        self.value = value

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        # Recursive fillin required to enable specialized storage strategies.
        pointers_w = g_self.pointers
        assert len(pointers_w) == 1
        pointers_w[0].fillin(space)
        self.value = space.unwrap_int(pointers_w[0].w_object)

    def has_class(self):
        return True

    def getclass(self, space):
        """Return Character from special objects array."""
        return space.w_Character

    def guess_classname(self):
        return "Character"

    def str_content(self):
        try:
            return "$" + chr(self.value)
        except ValueError:
            return "Character value: " + str(self.value)

    def gethash(self):
        return self.value

    def invariant(self):
        return isinstance(self.value, int)

    def _become(self, w_other):
        assert isinstance(w_other, W_Character)
        self.value, w_other.value = w_other.value, self.value
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def is_same_object(self, other):
        if not isinstance(other, W_Character):
            return False
        return self.value == other.value

    def __eq__(self, other):
        if not isinstance(other, W_Character):
            return False
        return self.value == other.value

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash(self.value)

    def clone(self, space):
        return self

    def unwrap_char_as_byte(self, space):
        return chr(self.value)

    def at0(self, space, index0):
        return self.fetch(space, index0)

    def atput0(self, space, index0, w_value):
        self.store(space, index0, w_value)

    def fetch(self, space, n0):
        if n0 != 0:
            raise IndexError
        return space.wrap_int(self.value)

    def store(self, space, n0, w_obj):
        if n0 != 0:
            raise IndexError
        if isinstance(w_obj, W_SmallInteger):
            self.value = w_obj.value
        else:
            raise IndexError

    def size(self):
        return 1

@signature.finishsigs
class W_AbstractObjectWithClassReference(W_AbstractObjectWithIdentityHash):
    """Objects with arbitrary class (ie not CompiledMethod, SmallInteger or
    Float)."""
    _attrs_ = ['w_class']
    _immutable_fields_ = ['w_class?']
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
            if self.getclass(None).has_space():
                class_shadow = self.class_shadow(self.getclass(None).space())
                return class_shadow.name
            else:
                # We cannot access the class during the initialization sequence.
                return "?? (class not initialized)"
        else:
            return "? (no class)"

    def change_class(self, space, w_class):
        self.w_class = w_class

    def invariant(self):
        from spyvm import storage_classes
        return (W_AbstractObjectWithIdentityHash.invariant(self) and
                isinstance(self.getclass(None).strategy, storage_classes.ClassShadow))

    def pointers_become_one_way(self, space, from_w, to_w):
        W_AbstractObjectWithIdentityHash.pointers_become_one_way(self, space, from_w, to_w)
        idx = 0
        try:
            idx = from_w.index(self.w_class)
        except ValueError:
            return
        w_class = self.w_class
        new_w_class = to_w[idx]
        assert isinstance(new_w_class, W_PointersObject)
        self.w_class = new_w_class
        w_class.post_become_one_way(new_w_class)

    def _become(self, w_other):
        assert isinstance(w_other, W_AbstractObjectWithClassReference)
        self.w_class, w_other.w_class = w_other.w_class, self.w_class
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def has_class(self):
        return self.getclass(None) is not None


class W_PointersObject(W_AbstractObjectWithIdentityHash):
    """Common object."""
    _attrs_ = ['strategy', '_storage']
    # TODO -- is it viable to have these as pseudo-immutable?
    # Measurably increases performance, since they do change rarely.
    _immutable_attrs_ = ['strategy?', '_storage?']
    strategy = None
    repr_classname = "W_PointersObject"
    rstrat.make_accessors(strategy='strategy', storage='_storage')

    @jit.unroll_safe
    def __init__(self, space, w_class, size, weak=False):
        """Create new object with size = fixed + variable size."""
        W_AbstractObjectWithIdentityHash.__init__(self)
        self._initialize_storage(space, w_class, size, weak)

    def _initialize_storage(self, space, w_class, size, weak=False):
        storage_type = space.strategy_factory.empty_storage_type(self, size, weak)
        space.strategy_factory.set_initial_strategy(self, storage_type, w_class, size)

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        # Recursive fillin required to enable specialized storage strategies.
        for g_obj in g_self.pointers:
            g_obj.fillin(space)
        pointers = g_self.get_pointers()
        storage_type = space.strategy_factory.strategy_type_for(pointers, weak=False) # do not fill in weak lists, yet
        space.strategy_factory.set_initial_strategy(self, storage_type, g_self.get_class(), len(pointers), pointers)

    def fillin_weak(self, space, g_self):
        assert g_self.isweak() # when we get here, this is true
        pointers = g_self.get_pointers()
        storage_type = space.strategy_factory.strategy_type_for(pointers, weak=True)
        space.strategy_factory.switch_strategy(self, storage_type)

    def is_weak(self):
        from storage import WeakListStrategy
        return isinstance(self._get_strategy(), WeakListStrategy)

    def getclass(self, space):
        if self._get_strategy() is None:
            return None
        else:
            return self._get_strategy().getclass()

    def has_class(self):
        return self.getclass(None) is not None

    def is_class(self, space):
        from spyvm.storage_classes import ClassShadow
        if isinstance(self._get_strategy(), ClassShadow):
            return True
        # XXX: copied form W_AbstractObjectWithClassReference
        if self.has_class():
            w_Metaclass = space.classtable["w_Metaclass"]
            w_class = self.getclass(space)
            if w_Metaclass.is_same_object(w_class):
                return True
            if w_class.has_class():
                return w_Metaclass.is_same_object(w_class.getclass(space))
        return False

    def change_class(self, space, w_class):
        old_strategy = self._get_strategy()
        new_strategy = old_strategy.instantiate(self, w_class)
        self._set_strategy(new_strategy)
        old_strategy._convert_storage_to(self, new_strategy)
        new_strategy.strategy_switched(self)

    def guess_classname(self):
        if self.has_class():
            if self.getclass(None).has_space():
                class_shadow = self.class_shadow(self.getclass(None).space())
                return class_shadow.name
            else:
                # We cannot access the class during the initialization sequence.
                return "?? (class not initialized)"
        else:
            return "? (no class)"

    def invariant(self):
        from spyvm import storage_classes
        return (W_AbstractObjectWithIdentityHash.invariant(self) and
                isinstance(self.getclass(None).strategy, storage_classes.ClassShadow))

    def assert_strategy(self):
        # Failing the following assert most likely indicates a bug. The strategy can only be absent during
        # the bootstrapping sequence. It will be initialized in the fillin() method. Before that, it should
        # not be switched to a specialized strategy, and the space is also not yet available here!
        # Otherwise, the specialized strategy will attempt to read information from an uninitialized object.
        strategy = self.strategy
        assert strategy, "The strategy has not been initialized yet!"
        return strategy

    def space(self):
        return self.assert_strategy().space

    def __str__(self):
        if self.has_strategy() and self.strategy.provides_getname:
            return self._get_strategy().getname()
        else:
            return W_AbstractObjectWithIdentityHash.__str__(self)

    def repr_content(self):
        strategy_info = "no strategy"
        name = ""
        if self.has_strategy():
            strategy_info = self.strategy.__repr__()
            if self.strategy.provides_getname:
                name = " [%s]" % self._get_strategy().getname()
        return '(%s) len=%d%s' % (strategy_info, self.size(), name)

    def unwrap_char(self, space):
        w_class = self.getclass(space)
        if not w_class.is_same_object(space.w_Character):
            raise error.UnwrappingError("expected Character")
        w_ord = self.fetch(space, constants.CHARACTER_VALUE_INDEX)
        if not isinstance(w_ord, W_SmallInteger):
            raise error.UnwrappingError("expected SmallInteger from Character")
        return chr(w_ord.value)

    @jit.look_inside_iff(lambda self, space: (
        (not self.class_shadow(space).isvariable()) or jit.isconstant(self.size())))
    def unwrap_array(self, space):
        # Check that our argument has pointers format and the class:
        if not self.getclass(space).is_same_object(space.w_Array):
            raise error.UnwrappingError
        return [self.at0(space, i) for i in range(self.size())]

    @jit.look_inside_iff(lambda self, space: (
        (not self.class_shadow(space).isvariable()) or jit.isconstant(self.size())))
    def fetch_all(self, space):
        return [self.fetch(space, i) for i in range(self.size())]

    @jit.look_inside_iff(lambda self, space, collection: (
        (not self.class_shadow(space).isvariable()) or len(collection) < 64))
    def store_all(self, space, collection):
        # Be tolerant: copy over as many elements as possible, set rest to nil.
        # The size of the object cannot be changed in any case.
        # TODO use store_all() provided by strategy?
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
        return self._get_strategy().fetch(self, n0)

    def store(self, space, n0, w_value):
        return self._get_strategy().store(self, n0, w_value)

    def size(self):
        if not self.has_strategy():
            # TODO - this happens only for objects bootstrapped in ObjSpace.
            # Think of a way to avoid this check. Usually, self.strategy is never None.
            return 0
        return self._get_strategy().size(self)

    def instsize(self):
        return self.class_shadow(self.space()).instsize()

    def store_strategy(self, strategy):
        self.strategy = strategy

    def _get_strategy(self):
        return self.strategy.promote_if_neccessary() if self.strategy is not None else None

    @objectmodel.specialize.arg(2)
    def as_special_get_shadow(self, space, TheClass):
        shadow = self._get_strategy()
        if not isinstance(shadow, TheClass):
            shadow = space.strategy_factory.switch_strategy(self, TheClass)
        assert isinstance(shadow, TheClass)
        return shadow

    def as_class_get_shadow(self, space):
        from spyvm.storage_classes import ClassShadow
        return jit.promote(self.as_special_get_shadow(space, ClassShadow))

    def as_context_get_shadow(self, space):
        from spyvm.storage_contexts import ContextPartShadow
        return self.as_special_get_shadow(space, ContextPartShadow)

    def as_methoddict_get_shadow(self, space):
        from spyvm.storage_classes import MethodDictionaryShadow
        return self.as_special_get_shadow(space, MethodDictionaryShadow)

    def as_cached_object_get_shadow(self, space):
        from spyvm.storage import CachedObjectShadow
        return self.as_special_get_shadow(space, CachedObjectShadow)

    def as_observed_get_shadow(self, space):
        from spyvm.storage import ObserveeShadow
        return self.as_special_get_shadow(space, ObserveeShadow)

    def has_strategy(self):
        return self._get_strategy() is not None

    def has_space(self):
        # The space is accessed through the strategy.
        return self.has_strategy()

    def _become(self, w_other):
        assert isinstance(w_other, W_PointersObject)
        # Only one strategy will handle the become (or none of them).
        # The receivers strategy gets the first shot.
        # If it doesn't want to, let the w_other's strategy handle it.
        if self.has_strategy() and self._get_strategy().handles_become():
            self.strategy.become(w_other)
        elif self.has_strategy() and self._get_strategy().handles_become():
            w_other.strategy.become(self)
        self.strategy, w_other.strategy = w_other.strategy, self.strategy
        self._storage, w_other._storage = w_other._storage, self._storage
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def pointers_become_one_way(self, space, from_w, to_w):
        ptrs = self.fetch_all(space)
        ptridx = 0
        for i, w_from in enumerate(from_w):
            try:
                ptridx = ptrs.index(w_from)
            except ValueError:
                continue
            w_to = to_w[i]
            ptrs[ptridx] = w_to
            w_from.post_become_one_way(w_to)
        self.store_all(space, ptrs)

    def clone(self, space):
        my_pointers = self.fetch_all(space)
        w_result = W_PointersObject(space, self.getclass(space), len(my_pointers))
        w_result.store_all(space, my_pointers)
        return w_result

class W_BytesObject(W_AbstractObjectWithClassReference):
    _attrs_ = ['version', 'bytes', 'native_bytes']
    repr_classname = 'W_BytesObject'
    bytes_per_slot = 1
    _immutable_fields_ = ['version?']

    def __init__(self, space, w_class, size):
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        assert isinstance(size, int)
        self.mutate()
        self.bytes = ['\x00'] * size
        self.native_bytes = None

    def mutate(self):
        self.version = Version()

    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        self.mutate()
        self.bytes = g_self.get_bytes()

    def at0(self, space, index0):
        return space.wrap_int(ord(self.getchar(index0)))

    def atput0(self, space, index0, w_value):
        self.setchar(index0, chr(space.unwrap_int(w_value)))

    def getchar(self, n0):
        if self.bytes is None:
            return self.native_bytes.getchar(n0)
        else:
            return self.bytes[n0]

    def setchar(self, n0, character):
        assert isinstance(character, str)
        assert len(character) == 1
        if self.bytes is None:
            self.native_bytes.setchar(n0, character)
        else:
            self.bytes[n0] = character
        self.mutate()

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
        if self.bytes is not None:
            return len(self.bytes)
        else:
            return self.native_bytes.size

    def str_content(self):
        if self.has_class() and self.getclass(None).has_space():
            if self.getclass(None).space().omit_printing_raw_bytes.is_set():
                return "<omitted>"
        return "'%s'" % ''.join([\
            char if ord(char) < 128 else (r'\x%s' % hex(ord(char))[2:]) for char in \
            (self.unwrap_string(None).replace('\r', '\n'))])

    def unwrap_string(self, space):
        return self._pure_as_string(self.version)

    @jit.elidable
    def _pure_as_string(self, version):
        if self.bytes is None:
            return self.native_bytes.as_string()
        else:
            return "".join(self.bytes)

    def getbytes(self):
        if self.bytes is None:
            return self.native_bytes.copy_bytes()
        else:
            return self.bytes

    def selector_string(self):
        return "#" + self.unwrap_string(None)

    def invariant(self):
        if not W_AbstractObjectWithClassReference.invariant(self):
            return False
        for c in self.bytes:
            if not isinstance(c, str) or len(c) != 1:
                return False
        return True

    def clone(self, space):
        size = self.size()
        w_result = W_BytesObject(space, self.getclass(space), size)
        if self.bytes is None:
            w_result.bytes = self.native_bytes.copy_bytes()
        else:
            w_result.bytes = list(self.bytes)
        return w_result

    @jit.unroll_safe
    def unwrap_uint(self, space):
        # TODO: Completely untested! This failed translation bigtime...
        # XXX Probably we want to allow all subclasses
        if not self.getclass(space).is_same_object(space.w_LargePositiveInteger):
            raise error.UnwrappingError("Failed to convert bytes to word")
        if self.size() > 4:
            raise error.UnwrappingError("Too large to convert bytes to word")
        word = r_uint(0)
        for i in range(self.size()):
            word += r_uint(ord(self.getchar(i))) << 8*i
        return word

    @jit.unroll_safe
    def unwrap_longlong(self, space):
        if self.size() > 8:
            raise error.UnwrappingError("Too large to convert bytes to word")
        word = r_longlong(0)
        for i in range(self.size()):
            try:
                word += r_longlong(ord(self.getchar(i))) << 8*i
            except OverflowError:
                raise error.UnwrappingError("Too large to convert bytes to word")
        if (space.w_LargeNegativeInteger is not None and
            self.getclass(space).is_same_object(space.w_LargeNegativeInteger)):
            return -word
        else:
            return word

    def unwrap_long_untranslated(self, space):
        "NOT_RPYTHON"
        if not we_are_translated():
            if self.size() >= 8:
                word = 0
                for i in range(self.size()):
                    word += ord(self.getchar(i)) << 8*i
                if (space.w_LargeNegativeInteger is not None and
                    self.getclass(space).is_same_object(space.w_LargeNegativeInteger)):
                    return -word
                else:
                    return word
        return self.unwrap_longlong(space)

    def is_array_object(self):
        return True

    def _become(self, w_other):
        assert isinstance(w_other, W_BytesObject)
        self.bytes, w_other.bytes = w_other.bytes, self.bytes
        self.native_bytes, w_other.native_bytes = w_other.native_bytes, self.native_bytes
        self.mutate()
        W_AbstractObjectWithClassReference._become(self, w_other)

    def convert_to_c_layout(self):
        if self.bytes is not None:
            self.native_bytes = NativeBytesWrapper(self.unwrap_string(None))
            self.bytes = None
            self.mutate()
        return self.native_bytes.c_bytes


# This indirection avoids a call for alloc_with_del in Jitted code
class NativeBytesWrapper(object):
    _attrs_ = ["c_bytes", "size"]
    _immutable_fields_ = ["c_bytes", "size"]
    def __init__(self, string):
        self.size = len(string)
        self.c_bytes = rffi.str2charp(string)

    def setchar(self, n0, char):
        self.c_bytes[n0] = char

    def getchar(self, n0):
        if n0 >= self.size:
            raise IndexError
        return self.c_bytes[n0]

    def as_string(self):
        return "".join([self.c_bytes[i] for i in range(self.size)])

    def copy_bytes(self):
        return [self.c_bytes[i] for i in range(self.size)]

    def __del__(self):
        rffi.free_charp(self.c_bytes)


class W_WordsObject(W_AbstractObjectWithClassReference):
    _attrs_ = ['words', 'native_words']
    repr_classname = "W_WordsObject"
    _immutable_fields_ = ['words?']

    def __init__(self, space, w_class, size):
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        self.words = [r_uint(0)] * size
        self.native_words = None

    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        self.words = g_self.get_ruints()

    def at0(self, space, index0):
        val = self.getword(index0)
        return space.wrap_uint(val)

    def atput0(self, space, index0, w_value):
        word = space.unwrap_uint(w_value)
        self.setword(index0, word)

    def getword(self, n):
        assert self.size() > n >= 0
        if self.words is None:
            return r_uint(self.native_words.getword(n))
        else:
            return self.words[n]

    def setword(self, n, word):
        if self.words is None:
            self.native_words.setword(n, intmask(word))
        else:
            self.words[n] = r_uint(word)

    def getchar(self, n0):
        return chr(self.getword(n0))

    def setchar(self, n0, character):
        assert isinstance(character, str)
        assert len(character) == 1
        self.setword(n0, ord(character))

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
        if self.words is not None:
            return len(self.words)
        else:
            return self.native_words.size

    @jit.look_inside_iff(lambda self, space: jit.isconstant(self.size()))
    def unwrap_string(self, space):
        # OH GOD! TODO: Make this sane!
        res = []
        for word in self.words:
            res += [chr(word & r_uint(0x000000ff)), chr((word & r_uint(0x0000ff00)) >> 8), chr((word & r_uint(0x00ff0000)) >> 16), chr((word & r_uint(0xff000000)) >> 24)]
        return "".join(res)

    def invariant(self):
        return (W_AbstractObjectWithClassReference.invariant(self) and
                isinstance(self.words, list))

    def clone(self, space):
        size = self.size()
        w_result = W_WordsObject(space, self.getclass(space), size)
        if self.words is None:
            w_result.words = self.native_words.copy_words()
        else:
            w_result.words = list(self.words)
        return w_result

    def is_array_object(self):
        return True

    def _become(self, w_other):
        assert isinstance(w_other, W_WordsObject)
        self.words, w_other.words = w_other.words, self.words
        self.native_words, w_other.native_words = w_other.native_words, self.native_words
        W_AbstractObjectWithClassReference._become(self, w_other)

    def convert_to_c_layout(self):
        if self.words is not None:
            self.native_words = NativeWordsWrapper(self.words)
            self.words = None
        return self.native_words.c_words


class NativeWordsWrapper(object):
    _attrs_ = ["c_words", "size"]
    _immutable_fields_ = ["c_words", "size"]

    def __init__(self, words):
        self.size = len(words)
        from spyvm.plugins.squeak_plugin_proxy import sqIntArrayPtr
        self.c_words = lltype.malloc(sqIntArrayPtr.TO, self.size, flavor='raw')
        for i in range(self.size):
            self.c_words[i] = intmask(words[i])

    def setword(self, n0, word):
        self.c_words[n0] = word

    def getword(self, n0):
        if n0 >= self.size:
            raise IndexError
        return self.c_words[n0]

    def copy_words(self):
        return [r_uint(self.c_words[i]) for i in range(self.size)]

    def __del__(self):
        lltype.free(self.c_words, flavor='raw')


class CompiledMethodHeader(object):
    def __init__(self, header_word):
        self.primitive_index = 0
        self.has_primitive = False
        self.number_of_literals = 0
        self.number_of_temporaries = 0
        self.number_of_arguments = 0
        self.large_frame = 0

class V3CompiledMethodHeader(CompiledMethodHeader):
    def __init__(self, header_word):
        self.primitive_index, self.number_of_literals, self.large_frame, \
                self.number_of_temporaries, self.number_of_arguments = \
                constants.decode_compiled_method_header(header_word)
        self.has_primitive = self.primitive_index != 0

class SpurCompiledMethodHeader(CompiledMethodHeader):
    def __init__(self, header_word):
        from spyvm.util.bitmanipulation import splitter
        self.number_of_literals, is_optimized_bit, has_primitive_bit, \
                self.large_frame, self.number_of_temporaries, \
                self.number_of_arguments, access_mod, instruction_set_bit = \
                splitter[15,1,1,1,6,4,2,1](header_word)
        self.has_primitive = has_primitive_bit == 1

    @staticmethod
    def has_primitive_bit_set(header_word):
        return header_word & (1 << 16) != 0

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
    _immutable_fields_ = ["version?"]
    lookup_selector = "<unknown>"
    lookup_class = None
    import_from_mixin(VersionMixin)

    def pointers_become_one_way(self, space, from_w, to_w):
        W_AbstractObjectWithIdentityHash.pointers_become_one_way(self, space, from_w, to_w)
        idx = -1
        try:
            idx = from_w.index(self.compiledin_class)
        except ValueError:
            pass
        if idx >= 0:
            compiledin_class = self.compiledin_class
            new_w_class = to_w[idx]
            assert isinstance(new_w_class, W_PointersObject)
            self.compiledin_class = new_w_class
            compiledin_class.post_become_one_way(new_w_class)
            self.changed()
        idx = -1
        try:
            idx = from_w.index(self.lookup_class)
        except ValueError:
            pass
        if idx >= 0:
            lookup_class = self.lookup_class
            new_w_class = to_w[idx]
            assert isinstance(new_w_class, W_PointersObject)
            self.lookup_class = new_w_class
            lookup_class.post_become_one_way(new_w_class)
            self.changed()

    def __init__(self, space, bytecount=0, header=0):
        self.bytes = ["\x00"] * bytecount
        self.setheader(space, header, initializing=True)

    def fillin(self, space, g_self):
        self.bytes = [] # make sure the attribute is defined
        # Implicitly sets the header, including self.literalsize
        for i, w_object in enumerate(g_self.get_pointers()):
            self.literalatput0(space, i, w_object, initializing=True)
        self.setbytes(g_self.get_bytes()[self.bytecodeoffset():])

    # === Setters ===

    def setheader(self, space, header, initializing=False):
        self.header = header

    def initialize_literals(self, number_of_literals, space, initializing=False):
        if initializing or self.literalsize != number_of_literals:
            # Keep the literals if possible.
            self.literalsize = number_of_literals
            self.literals = [space.w_nil] * self.literalsize

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
            self.setheader(space, header, initializing)
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
        copy = self.__class__(space, 0, self.getheader())
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

    def create_frame(self, space, receiver, arguments=[], s_fallback=None):
        from spyvm.storage_contexts import ContextPartShadow
        assert len(arguments) == self.argsize
        return ContextPartShadow.build_method_context(space, self, receiver, arguments, s_fallback=s_fallback)

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
            s_class = w_class.strategy
            if isinstance(s_class, ClassShadow):
                return "%s >> #%s" % (s_class.getname(), self.lookup_selector)
        return "#%s" % self.lookup_selector

class W_SpurCompiledMethod(W_CompiledMethod):
    """Handles the specialities of the method header in Spur"""

    def setheader(self, space, header, initializing=False):
        decoded_header = SpurCompiledMethodHeader(header)
        self.header = header
        self.initialize_literals(decoded_header.number_of_literals, space,
                initializing)
        self.argsize = decoded_header.number_of_arguments
        self._tempsize = decoded_header.number_of_temporaries
        self.islarge = decoded_header.large_frame
        self.compiledin_class = None
        if decoded_header.has_primitive and len(self.bytes) >= 3:
            self.update_primitive_index()
        else:
            self._primitive = 0
        self.changed()

    def setbytes(self, bytes):
        W_CompiledMethod.setbytes(self, bytes)
        if SpurCompiledMethodHeader.has_primitive_bit_set(self.header):
            self.update_primitive_index()

    def setchar(self, index0, character):
        W_CompiledMethod.setchar(self, index0, character)
        if index0 in (1, 2) and SpurCompiledMethodHeader.has_primitive_bit_set(self.header):
            self.update_primitive_index()

    def update_primitive_index(self):
        assert self.bytes[0] == chr(139)
        self._primitive = ord(self.bytes[1]) + (ord(self.bytes[2]) << 8)

class W_PreSpurCompiledMethod(W_CompiledMethod):

    def setheader(self, space, header, initializing=False):
        decoded_header = V3CompiledMethodHeader(header)
        self.header = header
        self.initialize_literals(decoded_header.number_of_literals, space,
                initializing)
        self.argsize = decoded_header.number_of_arguments
        self._tempsize = decoded_header.number_of_temporaries
        self._primitive = decoded_header.primitive_index
        self.islarge = decoded_header.large_frame
        self.compiledin_class = None
        self.changed()
