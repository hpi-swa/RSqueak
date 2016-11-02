import math

from rsqueakvm import constants, error
from rsqueakvm.model.base import W_Object, W_AbstractObjectWithIdentityHash

from rpython.rlib import longlong2float, jit
from rpython.rlib.rarithmetic import intmask, r_uint32, r_uint, ovfcheck, r_int64, r_ulonglong
from rpython.rlib.rstruct.ieee import float_unpack, float_pack
from rpython.rlib.objectmodel import compute_hash


class W_AbstractFloat(W_AbstractObjectWithIdentityHash):
    _attrs_ = []

    def __init__(self, value):
        raise NotImplementedError

    def fillin_fromwords(self, space, high, low):
        raise NotImplementedError

    def store(self, space, n0, w_obj):
        raise NotImplementedError

    def getvalue(self):
        raise NotImplementedError

    def setvalue(self, v):
        raise NotImplementedError

    def unwrap_string(self, space):
        word = longlong2float.float2longlong(self.getvalue())
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

    def getclass(self, space):
        """Return Float from special objects array."""
        return space.w_Float

    def guess_classname(self):
        return "Float"

    def str_content(self):
        return "%f" % self.getvalue()

    def gethash(self):
        return (intmask(compute_hash(self.getvalue())) % 2**22) + 1

    def _become(self, w_other):
        assert isinstance(w_other, W_AbstractFloat)
        self_value, w_other_value = self.getvalue(), w_other.getvalue()
        self.setvalue(w_other_value)
        w_other.setvalue(self_value)
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def is_same_object(self, other):
        if not isinstance(other, W_AbstractFloat):
            return False
        return ((self.getvalue() == other.getvalue()) or
                (math.isnan(self.getvalue()) and math.isnan(other.getvalue())))

    def __eq__(self, other):
        if not isinstance(other, W_AbstractFloat):
            return False
        return self.getvalue() == other.getvalue()

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash(self.getvalue())

    def clone(self, space):
        return self

    def unwrap_float(self, space):
        return self.getvalue()

    def at0(self, space, index0):
        return self.fetch(space, index0)

    def atput0(self, space, index0, w_value):
        self.store(space, index0, w_value)

    def fetch(self, space, n0):
        r = float_pack(self.getvalue(), 8)  # C double
        if n0 == 0:
            return space.wrap_uint(r_uint32(intmask(r >> 32)))
        else:
            # bounds-check for primitive access is done in the primitive
            assert n0 == 1
            if constants.IS_64BIT:
                # mask the bits above 32
                return space.wrap_uint(r_uint32(intmask(r & 0xffffffff)))
            else:
                return space.wrap_uint(r_uint32(intmask(r)))

    def size(self):
        return constants.WORDS_IN_FLOAT


class W_Float(W_AbstractFloat):
    """Boxed float value."""
    _attrs_ = ['value']
    _immutable_fields_ = ['value?']
    repr_classname = "W_Float"

    def __init__(self, value):
        self.value = value

    def fillin_fromwords(self, space, high, low):
        # TODO: support for larger float values?
        r = (r_ulonglong(high) << 32) | low
        self.value = float_unpack(r, 8)

    def store(self, space, n0, w_obj):
        uint = r_ulonglong(space.unwrap_uint(w_obj))
        r = float_pack(self.getvalue(), 8)
        if n0 == 0:
            r = ((r << 32) >> 32) | (uint << 32)
        else:
            assert n0 == 1
            r = ((r >> 32) << 32) | uint
        self.value = float_unpack(r, 8)

    def getvalue(self):
        return self.value

    def setvalue(self, v):
        self.value = v


class W_MutableFloat(W_AbstractFloat):
    """Balloon frequently converts 32-bit words into Floats from within the image.
    If we give a normal W_Float and let Balloon mutate the words to fill it in,
    we always have to abort any trace because we're forcing a quasi-immutable.
    So just for those cases where someone in the image is creating floats using
    `new' (in storage_classes.py), we use W_MutableFloat instances, which do not
    declare they're value as quasi-immutable."""
    _attrs_ = ["high", "low"]
    _immutable_fields_ = []
    repr_classname = "W_MutableFloat"

    def __init__(self, value):
        assert value == 0
        self.high = r_uint32(0)
        self.low = r_uint32(0)

    def store(self, space, n0, w_obj):
        """Floats are stored in big-endian (PowerPC) order"""
        if n0 == 0:
            self.high = r_uint32(space.unwrap_uint(w_obj))
        elif n0 == 1:
            self.low = r_uint32(space.unwrap_uint(w_obj))
        else:
            raise error.PrimitiveFailedError

    def getvalue(self):
        # Should be rarely called, probably only to do arithmetic once, and then
        # return a new W_Float.
        from rpython.rlib.rstruct.ieee import float_unpack
        return float_unpack((r_ulonglong(self.high) << 32) | r_ulonglong(self.low), 8)

    def setvalue(self, v):
        # This will only be called if we `become' a W_MutableFloat, should be rare
        r = float_pack(v, 8)
        # just shift and mask
        self.high = r_uint32(r >> 32)
        self.low = r_uint32(r)


class W_LargePositiveInteger1Word(W_AbstractObjectWithIdentityHash):
    """Large positive integer for exactly 1 word"""
    _attrs_ = ["value", "_exposed_size"]
    repr_classname = "W_LargePositiveInteger1Word"
    bytes_per_slot = 1

    def __init__(self, value, size=constants.BYTES_PER_MACHINE_INT):
        self.value = intmask(value)
        self._exposed_size = size

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        word = 0
        bytes = g_self.get_bytes()
        for idx, byte in enumerate(bytes):
            assert idx < constants.BYTES_PER_MACHINE_INT
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

    @jit.unroll_safe
    def unwrap_string(self, space):
        res = ['\0'] * self._exposed_size
        value = self.value
        mask = r_uint(0xff)
        for i in range(self._exposed_size):
            res[i] = chr(value & mask)
            value >>= 8
        return "".join(res)

    def lshift(self, space, shift):
        # shift > 0, therefore the highest bit of upperbound is not set,
        # i.e. upperbound is positive
        upperbound = intmask(r_uint(-1) >> shift)
        if 0 <= self.value <= upperbound:
            shifted = intmask(self.value << shift)
            return space.wrap_positive_wordsize_int(shifted)
        else:
            raise error.PrimitiveFailedError()

    def rshift(self, space, shift):
        if shift == 0:
            return self
        # a problem might arrise, because we may shift in ones from left
        mask = intmask((1 << (constants.LONG_BIT - shift)) - 1)
        # the mask is only valid if the highest bit of self.value is set
        # and only in this case we do need such a mask
        return space.wrap_int((self.value >> shift) & mask)

    def unwrap_int(self, space):
        if self.value >= 0:
            return intmask(self.value)
        else:
            raise error.UnwrappingError("The value is negative when "
                                        "interpreted as word-sized value.")

    def unwrap_uint(self, space):
        return r_uint(self.value)

    def unwrap_positive_wordsize_int(self, space):
        return r_uint(self.value)

    def unwrap_longlong(self, space):
        if not constants.IS_64BIT:
            retval = r_int64(r_uint(self.value))
        else:
            retval = intmask(r_uint(self.value))
        if retval < 0:
            raise error.UnwrappingError
        return retval

    def unwrap_long_untranslated(self, space):
        """NOT RPYTHON"""
        if not constants.IS_64BIT:
            return r_uint(self.value)
        else:
            return r_uint(self.value)

    def unwrap_rbigint(self, space):
        from rpython.rlib.rbigint import rbigint
        return rbigint.fromrarith_int(r_uint(self.value))

    def unwrap_float(self, space):
        return float(self.value)

    def clone(self, space):
        return W_LargePositiveInteger1Word(self.value)

    def at0(self, space, index0):
        shift = index0 * 8
        result = (self.value >> shift) & 0xff
        return space.wrap_smallint_unsafe(intmask(result))

    def atput0(self, space, index0, w_byte):
        self.setbyte(index0, space.unwrap_int(w_byte))

    def setchar(self, index0, char):
        self.setbyte(index0, ord(char))

    def setbyte(self, index0, byte):
        if index0 >= self.size():
            raise IndexError()
        skew = index0 * 8
        assert byte <= 0xff
        new_value = self.value & r_uint(~(0xff << skew))
        new_value |= r_uint(byte << skew)
        self.value = intmask(new_value)

    def size(self):
        return self._exposed_size

    def is_array_object(self):
        return True

    def _become(self, w_other):
        assert isinstance(w_other, W_LargePositiveInteger1Word)
        self.value, w_other.value = w_other.value, self.value
        self._exposed_size, w_other._exposed_size = (w_other._exposed_size,
                                                     self._exposed_size)
        W_AbstractObjectWithIdentityHash._become(self, w_other)


class W_SmallInteger(W_Object):
    """Boxed integer value"""
    _attrs_ = ['value']
    __slots__ = ('value',)     # the only allowed slot here
    _immutable_fields_ = ['value']
    repr_classname = "W_SmallInteger"

    def __init__(self, value):
        self.value = intmask(value)

    def getclass(self, space):
        return space.w_SmallInteger

    def gethash(self):
        return self.value

    def invariant(self):
        return isinstance(self.value, int)

    def lshift(self, space, shift):
        # shift > 0, therefore the highest bit of upperbound is not set,
        # i.e. upperbound is positive
        upperbound = intmask(r_uint(-1) >> shift)
        if 0 <= self.value <= upperbound:
            shifted = intmask(self.value << shift)
            return space.wrap_positive_wordsize_int(shifted)
        else:
            try:
                shifted = ovfcheck(self.value << shift)
            except OverflowError:
                raise error.PrimitiveFailedError()
            return space.wrap_int(shifted)
        raise error.PrimitiveFailedError()

    def rshift(self, space, shift):
        return space.wrap_int(self.value >> shift)

    def unwrap_int(self, space):
        return intmask(self.value)

    def unwrap_uint(self, space):
        val = self.value
        # Assume the caller knows what he does, even if int is negative
        return r_uint(val)

    def unwrap_positive_wordsize_int(self, space):
        if self.value >= 0:
            return r_uint(self.value)
        else:
            raise error.UnwrappingError

    def unwrap_longlong(self, space):
        return r_int64(self.value)

    def unwrap_rbigint(self, space):
        from rpython.rlib.rbigint import rbigint
        return rbigint.fromint(self.value)

    def unwrap_float(self, space):
        return float(self.value)

    def unwrap_char_as_byte(self, space):
        # We do not implement the STRING_REPLACE primitive, but some code paths
        # in Squeak rely on that primitive's munging of ByteArrays and
        # ByteStrings. We are forgiving, so we also allow bytes extraced from
        # ByteArrays to be unwrapped as characters and put into strings
        from rpython.rlib.rarithmetic import int_between
        value = self.value
        if not int_between(0, value, 256):
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

class W_MutableSmallInteger(W_SmallInteger):
    _attrs_ = ["value"]
    __slots__ = ('value',)     # the only allowed slot here
    _immutable_fields_ = []

    def set_value(self, v):
        self.value = intmask(v)
