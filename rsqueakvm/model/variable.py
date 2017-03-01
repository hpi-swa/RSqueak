from rsqueakvm import constants, error
from rsqueakvm.model.base import W_AbstractObjectWithClassReference
from rsqueakvm.util.version import Version, elidable_for_version_iff

from rpython.rlib import jit
from rpython.rlib.objectmodel import not_rpython
from rpython.rlib.rarithmetic import intmask, r_uint, r_uint32, r_int64


class W_BytesObject(W_AbstractObjectWithClassReference):
    _attrs_ = ['version', 'bytes']
    repr_classname = 'W_BytesObject'
    bytes_per_slot = 1
    _immutable_fields_ = ['version?']

    def __init__(self, space, w_class, size):
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        assert isinstance(size, int)
        self.mutate()
        self.bytes = ['\x00'] * size

    def mutate(self):
        self.version = Version()

    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        self.mutate()
        self.bytes = g_self.get_bytes()

    def at0(self, space, index0):
        return space.wrap_smallint_unsafe(ord(self.getchar(index0)))

    def atput0(self, space, index0, w_value):
        try:
            self.setchar(index0, chr(space.unwrap_int(w_value)))
        except ValueError: # when we try to put sth outside of chr range
            raise error.PrimitiveFailedError

    def getchar(self, n0):
        return self._bytes()[n0]

    def setchar(self, n0, character):
        assert isinstance(character, str)
        assert len(character) == 1
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
        return len(self._bytes())

    def _bytes(self):
        return self.bytes

    def _version(self):
        return self.version

    def str_content(self):
        if self.getclass(None).has_space():
            if self.getclass(None).space().omit_printing_raw_bytes.is_set():
                return "<omitted>"
        return "'%s'" % ''.join([\
            char if ord(char) < 128 else (r'\x%s' % hex(ord(char))[2:]) for char in \
            (self.unwrap_string(None).replace('\r', '\n'))])

    def unwrap_string(self, space):
        return self._pure_as_string(self._version())

    @jit.elidable
    def _pure_as_string(self, version):
        return "".join(self._bytes())

    def getbytes(self):
        return self._bytes()

    @jit.dont_look_inside
    def setbytes(self, lst):
        assert len(lst) == self.size()
        self.bytes = lst
        self.mutate()

    def is_positive(self, space):
        return self.getclass(space).is_same_object(space.w_LargePositiveInteger)

    @jit.unroll_safe
    def unwrap_uint(self, space):
        if not self.getclass(space).is_same_object(space.w_LargePositiveInteger):
            raise error.UnwrappingError("Invalid class for unwrapping byte object as uint")
        if self.size() > constants.BYTES_PER_MACHINE_INT:
            raise error.UnwrappingError("Too large to convert bytes to word")
        word = r_uint(0)
        for i in range(self.size()):
            word += r_uint(ord(self.getchar(i))) << 8*i
        return word

    @jit.unroll_safe
    def unwrap_int64(self, space):
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
        elif self.getclass(space).is_same_object(space.w_LargeNegativeInteger):
            return -word
        else:
            raise error.UnwrappingError

    def unwrap_rbigint(self, space):
        w_class = self.getclass(space)
        if w_class.is_same_object(space.w_LargePositiveInteger):
            return self.getrbigint()
        elif w_class.is_same_object(space.w_LargeNegativeInteger):
            return self.getrbigint().neg()
        else:
            raise error.UnwrappingError

    @not_rpython
    def unwrap_long_untranslated(self, space):
        return self.unwrap_rbigint(space).tolong()

    @elidable_for_version_iff(0, cond=lambda self: jit.isconstant(self))
    def getrbigint(self):
        from rpython.rlib.rbigint import rbigint
        return rbigint.frombytes(''.join(self._bytes()), 'little', False)

    def selector_string(self):
        return "#" + self.unwrap_string(None)

    def invariant(self):
        if not W_AbstractObjectWithClassReference.invariant(self):
            return False
        for c in self._bytes():
            if not isinstance(c, str) or len(c) != 1:
                return False
        return True

    def clone(self, space):
        size = self.size()
        w_result = W_BytesObject(space, self.getclass(space), size)
        w_result.bytes = list(self._bytes())
        return w_result

    def is_array_object(self):
        return True

    def _become(self, w_other):
        if not isinstance(w_other, W_BytesObject):
            raise error.PrimitiveFailedError
        self.bytes, w_other.bytes = w_other.bytes, self.bytes
        self.mutate()
        W_AbstractObjectWithClassReference._become(self, w_other)


class W_WordsObject(W_AbstractObjectWithClassReference):
    # TODO: this assumes only 32-bit words objects
    _attrs_ = ['words']
    repr_classname = "W_WordsObject"
    _immutable_fields_ = ['words?']

    def __init__(self, space, w_class, size):
        W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        self.words = [r_uint(0)] * size

    def fillin(self, space, g_self):
        W_AbstractObjectWithClassReference.fillin(self, space, g_self)
        self.words = g_self.get_ruints()

    def at0(self, space, index0):
        val = self.getword(index0)
        return space.wrap_int(val)

    def atput0(self, space, index0, w_value):
        word = space.unwrap_uint(w_value)
        self.setword(index0, word)

    def getword(self, n):
        assert self.size() > n >= 0
        return self._words()[n]

    def setword(self, n, word):
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

    @jit.dont_look_inside
    def setwords(self, lst):
        assert len(lst) == self.size()
        self.words = lst

    def getwords(self):
        return self.words

    def size(self):
        return len(self._words())

    def _words(self):
        return self.words

    @jit.look_inside_iff(lambda self, space: jit.isconstant(self.size()))
    def unwrap_string(self, space):
        # OH GOD! TODO: Make this sane!
        res = []
        for word in self._words():
            res += [chr((word & r_uint(0x000000ff)) >>  0),
                    chr((word & r_uint(0x0000ff00)) >>  8),
                    chr((word & r_uint(0x00ff0000)) >> 16),
                    chr((word & r_uint(0xff000000)) >> 24)]
        return "".join(res)

    def invariant(self):
        return (W_AbstractObjectWithClassReference.invariant(self) and
                isinstance(self._words(), list))

    def clone(self, space):
        size = self.size()
        w_result = W_WordsObject(space, self.getclass(space), size)
        w_result.words = list(self._words())
        return w_result

    def is_array_object(self):
        return True

    def _become(self, w_other):
        if not isinstance(w_other, W_WordsObject):
            raise error.PrimitiveFailedError
        self.words, w_other.words = w_other.words, self.words
        W_AbstractObjectWithClassReference._become(self, w_other)

    def convert_to_bytes_layout(self, wordsize):
        words = self.words
        new_words = [r_uint(0)] * len(words * wordsize)
        for i, word in enumerate(words):
            new_words[i * 4 + 0] = r_uint((word >> 0) & 0xff)
            new_words[i * 4 + 1] = r_uint((word >> 8) & 0xff)
            new_words[i * 4 + 2] = r_uint((word >> 16) & 0xff)
            new_words[i * 4 + 3] = r_uint((word >> 24) & 0xff)
        self.words = new_words
        return self
