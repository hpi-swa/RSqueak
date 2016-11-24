import math

from rsqueakvm import constants, error
from rsqueakvm.model.base import W_Object, W_AbstractObjectWithIdentityHash, W_AbstractObjectWithClassReference

from rpython.rlib import longlong2float, jit
from rpython.rlib.rarithmetic import intmask, r_uint32, r_uint, ovfcheck, r_int64, r_ulonglong
from rpython.rlib.rstruct.ieee import float_unpack, float_pack
from rpython.rlib.objectmodel import compute_hash, specialize
from rpython.rlib import rbigint
from rpython.rlib.rerased import new_static_erasing_pair


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
        if isinstance(w_other, W_AbstractFloat):
            raise error.PrimitiveFailedError
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
            return space.wrap_int(r_uint32(intmask(r >> 32)))
        else:
            # bounds-check for primitive access is done in the primitive
            assert n0 == 1
            if constants.IS_64BIT:
                # mask the bits above 32
                return space.wrap_int(r_uint32(intmask(r & 0xffffffff)))
            else:
                return space.wrap_int(r_uint32(intmask(r)))

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


class W_LargeInteger(W_AbstractObjectWithClassReference):
    _attrs_ = ["_exposed_size"]
    _immutable_fields_ = ["_exposed_size"]
    repr_classname = "W_LargeInteger"
    bytes_per_slot = 1

    def __init__(self, space, w_class, size):
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        self._exposed_size = size

    def size(self):
        return self._exposed_size

    def is_array_object(self):
        return True

    def is_positive(self, space):
        return self.getclass(space).is_same_object(space.w_LargePositiveInteger)


class W_LargeIntegerBig(W_LargeInteger):
    """Large integer using rbigints"""
    _attrs_ = ["value"]
    _immutable_fields_ = ["value?"]
    repr_classname = "W_LargeIntegerBig"

    def __init__(self, space, w_class, value, size=0):
        W_LargeInteger.__init__(self, space, w_class, size)
        self.value = value

    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        bytes = g_self.get_bytes()
        value = rbigint.rbigint.frombytes(bytes, 'little', False)
        if self.getclass(space).is_same_object(space.w_LargePositiveInteger):
            self.value = value
        else:
            self.value = value.neg()
        self._exposed_size = len(bytes)

    def str_content(self):
        return self.value.str()

    def size(self):
        if jit.isconstant(self):
            self.calculate_exposed_size_for_big_int_once()
            return self._exposed_size
        else:
            return self.calculate_exposed_size_for_big_int_call()

    # this method is elidable so the jit won't look inside to notice that we're
    # setting an immutable field. This should be ok, because size is hopefully
    # requested rarely for these numbers.
    @jit.elidable
    def calculate_exposed_size_for_big_int_call(self):
        if self._exposed_size == 0:
            import math
            bytelenval = self.value.abs()
            # XXX +0.05: heuristic hack float rounding errors
            bytelen = int(math.floor(bytelenval.log(256) + 0.05)) + 1
            # TODO: check if this might be better
            # try:
            #     bytes = val.tobytes(bytelen, 'little', False)
            # except OverflowError:
            #     # round-off errors in math.log, might need an extra byte
            #     bytes = val.tobytes(bytelen + 1, 'little', False)
            self._exposed_size = bytelen
        return self._exposed_size

    # if self is a constant, we just ensure we calculate the exposed_size without a
    # residual call in the trace, if it isn't we always insert the call
    @jit.not_in_trace
    def calculate_exposed_size_for_big_int_once(self):
        self.calculate_exposed_size_for_big_int_call()

    def unwrap_string(self, space):
        return self.value.abs().tobytes(self.size(), 'little', False)

    def at0(self, space, n0):
        return space.wrap_int(ord(self.unwrap_string(space)[n0]))

    def atput0(self, space, n0, w_value):
        bytes = list(self.unwrap_string(space))
        try:
            bytes[n0] = chr(space.unwrap_int(w_value))
        except ValueError: # when we try to put sth outside of chr range
            raise error.PrimitiveFailedError
        value = rbigint.rbigint.frombytes(bytes, 'little', False)
        if self.getclass(space).is_same_object(space.w_LargePositiveInteger):
            self.value = value
        else:
            self.value = value.neg()

    def unwrap_int(self, space):
        try:
            return self.value.toint()
        except OverflowError:
            raise error.UnwrappingError

    def unwrap_uint(self, space):
        try:
            return self.value.abs().touint()
        except OverflowError:
            raise error.UnwrappingError

    def unwrap_int64(self, space):
        try:
            return self.value.tolonglong()
        except OverflowError:
            raise error.UnwrappingError

    def unwrap_rbigint(self, space):
        return self.value

    def unwrap_long_untranslated(self, space):
        "NOT RPYTHON"
        return self.value.tolong()

    def unwrap_float(self, space):
        return self.value.tofloat()

    def clone(self, space):
        return W_LargeIntegerBig(space, self.getclass(space), self.value, self.size())

    @jit.dont_look_inside
    def _become(self, w_other):
        if not isinstance(w_other, W_LargeIntegerBig):
            raise error.PrimitiveFailedError
        self.value, w_other.value = w_other.value, self.value
        self._exposed_size, w_other._exposed_size = (w_other._exposed_size,
                                                     self._exposed_size)
        W_AbstractObjectWithClassReference._become(self, w_other)


class W_LargeIntegerWord(W_LargeInteger):
    _attrs_ = ["value"]
    _immutable_fields_ = ["value?"]
    repr_classname = "W_LargeIntegerWord"

    def __init__(self, space, w_class, value, size):
        W_LargeInteger.__init__(self, space, w_class, size)
        self.value = value

    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        bytes = g_self.get_bytes()
        value = rbigint.rbigint.frombytes(bytes, 'little', False)
        self.value = value.touint()
        self._exposed_size = len(bytes)

    def str_content(self):
        return str(self.value)

    def unwrap_string(self, space):
        out = []
        val = self.value
        for i in range(self.size()):
            out.append(chr(val & r_uint(0xFF)))
            val >>= 8
        return "".join(out)

    def at0(self, space, n0):
        shift = n0 * 8
        result = (self.value >> shift) & 0xff
        return space.wrap_smallint_unsafe(intmask(result))

    def atput0(self, space, n0, w_value):
        if n0 >= self.size():
            raise IndexError
        skew = n0 * 8
        byte = space.unwrap_int(w_value)
        if byte > 0xff:
            raise error.PrimitiveFailedError
        new_value = self.value & r_uint(~(0xff << skew))
        new_value |= r_uint(byte << skew)
        self.value = new_value

    def unwrap_uint(self, space):
        return r_uint(self.value)

    def unwrap_int64(self, space):
        if constants.IS_64BIT:
            ret = intmask(self.value)
            if self.is_positive(space):
                if ret < 0: raise error.UnwrappingError
            else:
                if ret > 0: raise error.UnwrappingError
        else:
            ret = r_int64(self.value)
            if self.is_positive(space):
                return ret
            else:
                return -ret

    def unwrap_rbigint(self, space):
        rbig = rbigint.rbigint.fromrarith_int(self.value)
        if not self.is_positive(space):
            return rbig.neg()
        else:
            return rbig

    def unwrap_long_untranslated(self, space):
        "NOT RPYTHON"
        if self.is_positive(space):
            return long(self.value)
        else:
            return -long(self.value)

    def unwrap_float(self, space):
        if self.is_positive(space):
            return float(self.value)
        raise error.UnwrappingError

    def clone(self, space):
        return W_LargeIntegerWord(space, self.getclass(space), self.value, self.size())

    @jit.dont_look_inside
    def _become(self, w_other):
        if not isinstance(w_other, W_LargeIntegerWord):
            raise error.PrimitiveFailedError
        self.value, w_other.value = w_other.value, self.value
        self._exposed_size, w_other._exposed_size = (w_other._exposed_size,
                                                     self._exposed_size)
        W_AbstractObjectWithClassReference._become(self, w_other)


class W_SmallInteger(W_Object):
    """Boxed integer value"""
    _attrs_ = ['value']
    __slots__ = ('value',)     # the only allowed slot here
    _immutable_fields_ = ['value']
    repr_classname = "W_SmallInteger"

    def __init__(self, value):
        self.value = intmask(value)

    def fillin(self, space, g_self):
        "This is only called for Large Integers that for us fit in SmallIntegers"
        bytes = g_self.get_bytes()
        value = rbigint.rbigint.frombytes(bytes, 'little', False)
        w_prev_class = g_self.get_class()
        if not w_prev_class.is_same_object(space.w_LargePositiveInteger):
            value = value.neg()
        self.value = value.toint()

    def getclass(self, space):
        return space.w_SmallInteger

    def gethash(self):
        return self.value

    def invariant(self):
        return isinstance(self.value, int)

    def unwrap_int(self, space):
        return intmask(self.value)

    def unwrap_uint(self, space):
        return r_uint(self.value)

    def unwrap_int64(self, space):
        return r_int64(self.value)

    def unwrap_rbigint(self, space):
        return rbigint.rbigint.fromint(self.value)

    def unwrap_long_untranslated(self, space):
        "NOT RPYTHON"
        return self.value

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

    def is_positive(self, space):
        return self.value >= 0

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
