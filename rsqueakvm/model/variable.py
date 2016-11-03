from rsqueakvm import constants, error
from rsqueakvm.model.base import W_AbstractObjectWithClassReference
from rsqueakvm.util.version import Version

from rpython.rlib import jit
from rpython.rlib.rarithmetic import intmask, r_uint, r_uint32, r_int64
from rpython.rlib.objectmodel import we_are_translated, always_inline
from rpython.rtyper.lltypesystem import lltype, rffi


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
        self.native_bytes = None

    def at0(self, space, index0):
        return space.wrap_smallint_unsafe(ord(self.getchar(index0)))

    def atput0(self, space, index0, w_value):
        try:
            self.setchar(index0, chr(space.unwrap_int(w_value)))
        except ValueError: # when we try to put sth outside of chr range
            raise error.PrimitiveFailedError

    def getchar(self, n0):
        if self.native_bytes is not None:
            return self.native_bytes.getchar(n0)
        else:
            return self.bytes[n0]

    def setchar(self, n0, character):
        assert isinstance(character, str)
        assert len(character) == 1
        if self.native_bytes is not None:
            self.native_bytes.setchar(n0, character)
        else:
            self.bytes[n0] = character
        self.mutate()

    def short_at0(self, space, index0):
        byte_index0 = index0 * 2
        byte0 = ord(self.getchar(byte_index0))
        byte1 = ord(self.getchar(byte_index0 + 1)) << 8
        if byte1 & 0x8000 != 0:
            byte1 = intmask(r_uint(r_uint32(0xffff0000)) | r_uint(r_uint32(byte1)))
        return space.wrap_smallint_unsafe(byte1 | byte0)

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
        if self.native_bytes is not None:
            return self.native_bytes.size
        else:
            return len(self.bytes)

    def str_content(self):
        if self.getclass(None).has_space():
            if self.getclass(None).space().omit_printing_raw_bytes.is_set():
                return "<omitted>"
        return "'%s'" % ''.join([\
            char if ord(char) < 128 else (r'\x%s' % hex(ord(char))[2:]) for char in \
            (self.unwrap_string(None).replace('\r', '\n'))])

    def unwrap_string(self, space):
        return self._pure_as_string(self.version)

    @jit.elidable
    def _pure_as_string(self, version):
        if self.native_bytes is not None:
            return self.native_bytes.as_string()
        else:
            return "".join(self.bytes)

    def getbytes(self):
        if self.native_bytes is not None:
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
        if self.native_bytes is not None:
            w_result.bytes = self.native_bytes.copy_bytes()
        else:
            w_result.bytes = list(self.bytes)
        return w_result

    @jit.unroll_safe
    def unwrap_uint(self, space):
        if not self.getclass(space).is_same_object(space.w_LargePositiveInteger):
            raise error.UnwrappingError("Failed to convert bytes to word")
        if self.size() > constants.BYTES_PER_MACHINE_INT:
            raise error.UnwrappingError("Too large to convert bytes to word")
        word = r_uint(0)
        for i in range(self.size()):
            word += r_uint(ord(self.getchar(i))) << 8*i
        return word

    @jit.unroll_safe
    def unwrap_longlong(self, space):
        if self.size() > constants.BYTES_PER_MACHINE_LONGLONG:
            raise error.UnwrappingError("Too large to convert bytes to word")
        elif (self.size() == constants.BYTES_PER_MACHINE_LONGLONG and
              ord(self.getchar(constants.BYTES_PER_MACHINE_LONGLONG - 1)) >= 0x80):
            # Sign-bit is set, this will overflow
            raise error.UnwrappingError("Too large to convert bytes to word")
        word = r_int64(0)
        for i in range(self.size()):
            try:
                word += r_int64(ord(self.getchar(i))) << 8*i
            except OverflowError: # never raised after translation :(
                raise error.UnwrappingError("Too large to convert bytes to word")
        if self.getclass(space).is_same_object(space.w_LargePositiveInteger):
            return word
        elif ((space.w_LargeNegativeInteger is not None) and
              self.getclass(space).is_same_object(space.w_LargeNegativeInteger)):
            return -word
        else:
            raise error.UnwrappingError

    def unwrap_long_untranslated(self, space):
        "NOT_RPYTHON"
        if not we_are_translated():
            if self.size() >= constants.BYTES_PER_MACHINE_LONGLONG:
                word = 0
                for i in range(self.size()):
                    word += ord(self.getchar(i)) << 8*i
                if (space.w_LargeNegativeInteger is not None and
                    self.getclass(space).is_same_object(space.w_LargeNegativeInteger)):
                    return -word
                else:
                    return word
        return self.unwrap_longlong(space)

    def unwrap_rbigint(self, space):
        if self.getclass(space).is_same_object(space.w_LargePositiveInteger):
            return self.getrbigint(self.version)
        elif ((space.w_LargeNegativeInteger is not None) and
              self.getclass(space).is_same_object(space.w_LargeNegativeInteger)):
            return self.getrbigint(self.version).neg()
        else:
            raise error.UnwrappingError

    @jit.elidable
    def getrbigint(self, version):
        from rpython.rlib.rbigint import rbigint
        return rbigint.frombytes(self.getbytes(), 'little', False)

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
    # TODO: this assumes only 32-bit words objects
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
        self.native_words = None

    def at0(self, space, index0):
        val = self.getword(index0)
        return space.wrap_uint(val)

    def atput0(self, space, index0, w_value):
        word = space.unwrap_uint(w_value)
        self.setword(index0, word)

    def getword(self, n):
        assert self.size() > n >= 0
        if self.native_words is not None:
            return r_uint(self.native_words.getword(n))
        else:
            return self.words[n]

    def setword(self, n, word):
        if self.native_words is not None:
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
        return space.wrap_smallint_unsafe(intmask(short))

    def short_atput0(self, space, index0, w_value):
        from rpython.rlib.rarithmetic import int_between
        i_value = space.unwrap_int(w_value)
        if not int_between(-0x8000, i_value, 0x8000):
            raise error.PrimitiveFailedError
        word_index0 = index0 / 2
        word = intmask(self.getword(word_index0))
        if not constants.IS_64BIT:
            if index0 % 2 == 0:
                word = intmask(r_uint(word) & r_uint(0xffff0000)) | (i_value & 0xffff)
            else:
                word = (i_value << 16) | (word & 0xffff)
            value = r_uint(word)
        else:
            if index0 % 2 == 0:
                word = intmask(r_uint(word & 0xffffffff) & r_uint(0xffff0000)) | (i_value & 0xffff)
            else:
                word = (i_value << 16) | (word & 0xffff)
            value = r_uint(word & 0xffffffff)
        self.setword(word_index0, value)

    def size(self):
        if self.native_words is not None:
            return self.native_words.size
        else:
            return len(self.words)

    @jit.look_inside_iff(lambda self, space: jit.isconstant(self.size()))
    def unwrap_string(self, space):
        # OH GOD! TODO: Make this sane!
        res = []
        for word in self.words:
            res += [chr((word & r_uint(0x000000ff)) >>  0),
                    chr((word & r_uint(0x0000ff00)) >>  8),
                    chr((word & r_uint(0x00ff0000)) >> 16),
                    chr((word & r_uint(0xff000000)) >> 24)]
        return "".join(res)

    def invariant(self):
        return (W_AbstractObjectWithClassReference.invariant(self) and
                isinstance(self.words, list))

    def clone(self, space):
        size = self.size()
        w_result = W_WordsObject(space, self.getclass(space), size)
        if self.native_words is not None:
            assert isinstance(self.native_words, NativeWordsWrapper)
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
        assert isinstance(self.native_words, NativeWordsWrapper)
        return self.native_words.c_words

    def convert_to_bytes_layout(self, wordsize):
        if self.words is not None:
            self.native_words = NativeWordsAsBytesWrapper(self.words, len(self.words) * wordsize)
            self.words = None
        else:
            raise error.PrimitiveFailedError
        return self


class AbstractNativeWordsWrapper(object):
    _attrs_ = ["size"]
    _immutable_fields_ = ["size"]


class NativeWordsWrapper(AbstractNativeWordsWrapper):
    _attrs_ = ["c_words"]
    _immutable_fields_ = ["c_words"]

    def __init__(self, words):
        self.size = len(words)
        from rsqueakvm.plugins.iproxy import sqIntArrayPtr
        self.c_words = lltype.malloc(sqIntArrayPtr.TO, self.size, flavor='raw')
        for i in range(self.size):
            self.c_words[i] = rffi.r_int(words[i])

    def setword(self, n0, word):
        self.c_words[n0] = rffi.r_int(word)

    def getword(self, n0):
        if n0 >= self.size:
            raise IndexError
        return r_uint(self.c_words[n0])

    def copy_words(self):
        return [r_uint(self.c_words[i]) for i in range(self.size)]

    def __del__(self):
        lltype.free(self.c_words, flavor='raw')



@always_inline
def r_char(x):
    return rffi.cast(rffi.CHAR, x)


class NativeWordsAsBytesWrapper(AbstractNativeWordsWrapper):
    """This is a terrible hack. See SmalltalkImage>>calcEndianess in modern Spur images. -tfel"""
    _attrs_ = ["c_bytes"]
    _immutable_fields_ = ["c_bytes"]

    def __init__(self, words, size):
        self.size = size
        self.c_bytes = lltype.malloc(rffi.CCHARP.TO, self.size, flavor='raw')
        for i, word in enumerate(words):
            self.c_bytes[i * 4] = r_char(word)
            self.c_bytes[i * 4 + 1] = r_char(word >> 8)
            self.c_bytes[i * 4 + 2] = r_char(word >> 16)
            self.c_bytes[i * 4 + 3] = r_char(word >> 24)

    def setword(self, n0, word):
        self.c_bytes[n0] = r_char(word)

    def getword(self, n0):
        if n0 >= self.size:
            raise IndexError
        return ord(self.c_bytes[n0])

    def __del__(self):
        lltype.free(self.c_bytes, flavor='raw')
