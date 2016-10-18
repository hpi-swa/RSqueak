import math

from rsqueakvm import constants, error
from rsqueakvm.model.base import W_Object, W_AbstractObjectWithIdentityHash

from rpython.rlib import longlong2float, jit, rbigint
from rpython.rlib.rarithmetic import intmask, r_uint32, r_uint, ovfcheck, r_int64
from rpython.rlib.objectmodel import compute_hash


BASE255 = "".join([chr(i) for i in range(256)])


class W_Float(W_AbstractObjectWithIdentityHash):
    """Boxed float value."""
    _attrs_ = ['value']
    _immutable_fields_ = ['value?']
    repr_classname = "W_Float"

    def fillin_fromwords(self, space, high, low):
        from rpython.rlib.rstruct.ieee import float_unpack
        from rpython.rlib.rarithmetic import r_ulonglong
        # TODO: support for larger float values?
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

    def getclass(self, space):
        """Return Float from special objects array."""
        return space.w_Float

    def guess_classname(self):
        return "Float"

    def str_content(self):
        return "%f" % self.value

    def gethash(self):
        return (intmask(compute_hash(self.value)) % 2**22) + 1

    def invariant(self):
        return isinstance(self.value, float)

    def _become(self, w_other):
        assert isinstance(w_other, W_Float)
        self.value, w_other.value = w_other.value, self.value
        W_AbstractObjectWithIdentityHash._become(self, w_other)

    def is_same_object(self, other):
        if not isinstance(other, W_Float):
            return False
        return ((self.value == other.value) or
                (math.isnan(self.value) and math.isnan(other.value)))

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
        r = float_pack(self.value, 8)  # C double
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


class W_LargeInteger(W_AbstractObjectWithIdentityHash):
    """Large integer, stored as rbigint"""
    _attrs_ = ["value"]
    repr_classname = "W_LargeInteger"
    bytes_per_slot = 1

    def __init__(self, value):
        self.value = value

    def fillin(self, space, g_self):
        W_AbstractObjectWithIdentityHash.fillin(self, space, g_self)
        bytes = g_self.get_bytes()
        self.value = rbigint.rbigint.frombytes(bytes, 'little', False)
        if not g_self.g_class.w_object.is_same_object(space.w_LargePositiveInteger):
            self.value = self.value.neg()

    def getclass(self, space):
        if self.value.sign > 0:
            return space.w_LargePositiveInteger
        else:
            return space.w_LargeNegativeInteger

    def guess_classname(self):
        if self.value.sign > 0:
            return "LargePositiveInteger"
        else:
            return "LargeNegativeInteger"

    def invariant(self):
        return isinstance(self.value, rbigint.rbigint)

    def str_content(self):
        return self.value.str()

    def unwrap_string(self, space):
        return self.value.str()

    def lshift(self, space, shift):
        return W_LargeInteger(self.value.lshift(shift))

    def rshift(self, space, shift):
        return space.wrap_int(self.value.rshift(shift))

    def unwrap_int(self, space):
        try:
            return self.value.toint()
        except OverflowError:
            raise error.UnwrappingError("LargeInteger does not fit in int")

    def unwrap_uint(self, space):
        try:
            return self.value.touint()
        except (ValueError, OverflowError):
            raise error.UnwrappingError("LargeInteger does not fit in uint")

    def unwrap_positive_wordsize_int(self, space):
        try:
            return self.value.uintmask()
        except OverflowError:
            raise error.UnwrappingError("LargeInteger does not fit in uint")

    def unwrap_longlong(self, space):
        try:
            return self.value.tolonglong()
        except OverflowError:
            raise error.UnwrappingError

    def unwrap_float(self, space):
        return self.value.tofloat()

    def clone(self, space):
        return W_LargeInteger(self.value)

    def _getbytes(self):
        v = self.value if self.value.sign > 0 else self.value.neg()
        return v.tobytes(self.size(), 'little', False)

    def _setbytes(self, bytes):
        v = rbigint.rbigint.frombytes(bytes, 'little', False)
        if self.value.sign < 0:
            v = v.neg()
        self.value = v

    def at0(self, space, index0):
        return space.wrap_smallint_unsafe(ord(self._getbytes()[index0]))

    def atput0(self, space, index0, w_byte):
        self.setbyte(index0, space.unwrap_int(w_byte))

    def setchar(self, index0, char):
        self.setbyte(index0, ord(char))

    def setbyte(self, index0, byte):
        if index0 >= self.size():
            raise IndexError()
        bytes = self._getbytes()
        bytes[index0] = space.unwrap_int(w_byte)
        self._setbytes(bytes)

    def size(self):
        return math.trunc(self.value.log(255)) + 1

    def is_array_object(self):
        return True

    def _become(self, w_other):
        assert isinstance(w_other, W_LargeInteger)
        self.value, w_other.value = w_other.value, self.value
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
