
from spyvm import model, constants, display
from spyvm.util import system
from rpython.rlib import jit, objectmodel
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rlib.rarithmetic import r_uint


def from_words_object(w_obj, form):
    depth = form.depth()
    space = form.space
    size = w_obj.size()
    w_class = w_obj.getclass(space)

    if depth < 8:
        w_display_bitmap = W_MappingDisplayBitmap(space, w_class, size, depth)
    elif depth == 8:
        w_display_bitmap = W_8BitDisplayBitmap(space, w_class, size, depth)
    elif depth == 16:
        w_display_bitmap = W_16BitDisplayBitmap(space, w_class, size, depth)
    else:
        assert depth == 32
        w_display_bitmap = W_32BitDisplayBitmap(space, w_class, size, depth)

    for idx in range(size):
        w_display_bitmap.setword(idx, w_obj.getword(idx))

    return w_display_bitmap

class W_DisplayBitmap(model.W_AbstractObjectWithClassReference):
    _attrs_ = ['pixelbuffer_words', '_real_depth_buffer', '_realsize', '_display', '_depth']
    _immutable_fields_ = ['pixelbuffer_words?', '_real_depth_buffer', '_realsize', '_display', '_depth']
    repr_classname = "W_DisplayBitmap"

    def __init__(self, space, w_class, size, depth):
        model.W_AbstractObjectWithClassReference.__init__(self, space, w_class)
        self._real_depth_buffer = lltype.malloc(rffi.CArray(rffi.UINT), size, flavor='raw')
        self._realsize = size
        self._depth = depth
        self._display = space.display()
        self.relinquish_display()

    # === Object access

    def at0(self, space, index0):
        self = jit.promote(self)
        val = self.getword(index0)
        return space.wrap_uint(val)

    def atput0(self, space, index0, w_value):
        self = jit.promote(self)
        word = space.unwrap_uint(w_value)
        self.setword(index0, word)

    def unwrap_string(self, space):
        # OH GOD! TODO: Make this sane!
        res = []
        for i in range(self._realsize):
            res += [chr(self.getword(i) & r_uint(0x000000ff)),
                    chr((self.getword(i) & r_uint(0x0000ff00)) >> 8),
                    chr((self.getword(i) & r_uint(0x00ff0000)) >> 16),
                    chr((self.getword(i) & r_uint(0xff000000)) >> 24)]
        return "".join(res)

    def getword(self, n):
        assert self.size() > n >= 0
        return self._real_depth_buffer[n]

    def setword(self, n, word):
        self._real_depth_buffer[n] = word

    def size(self):
        return self._realsize

    # === Graphics

    def display(self):
        return jit.promote(self._display)

    def pixelbuffer(self):
        return self.display().get_pixelbuffer()

    def pixelbuffer_UCHAR(self):
        return self.display().get_pixelbuffer_UCHAR()

    def set_pixelbuffer_word(self, n, word):
        self.pixelbuffer()[n] = word

    @jit.elidable
    def pixel_per_word(self):
        return constants.BYTES_PER_WORD / (self.display().depth / 8)

    def take_over_display(self):
        # Make sure FrameWrapper.take_over_display() is called first for the correct Frame object.
        self.pixelbuffer_words = self.display().width * self.display().height / self.pixel_per_word()
        self.update_from_buffer()

    def relinquish_display(self):
        self.pixelbuffer_words = 0

    def flush_to_screen(self):
        self.display().flip()

    def word_from_pixel(self, x, y):
        return (x + y * self.display().width) / self.pixel_per_word()

    def force_rectange_to_screen(self, left, right, top, bottom):
        if self.pixelbuffer_words > 0:
            start = max(self.word_from_pixel(left, top), 0)
            stop = min(self.word_from_pixel(right, bottom), self.size() - 1)
            if stop < start:
                return
            self.force_words(start, stop)

    def force_words(self, start, stop):
        for i in range(stop - start):
            self.set_pixelbuffer_word(i + start, self.getword(i + start))

    def update_from_buffer(self):
        if self.pixelbuffer_words > 0:
            for i in range(self.size()):
                self.set_pixelbuffer_word(i, self.getword(i))

    # === Misc

    def invariant(self):
        return False

    def clone(self, space):
        w_result = model.W_WordsObject(space, self.getclass(space), self.size())
        for n in range(self.size()):
            w_result.setword(n, self.getword(n))
        return w_result

    def is_array_object(self):
        return True

    def can_become(self, w_other):
        # TODO - implement _become() for this class. Impossible due to _immutable_fields_?
        return False

    def convert_to_c_layout(self):
        return self._real_depth_buffer

    def __del__(self):
        lltype.free(self._real_depth_buffer, flavor='raw')

    def repr_content(self):
        return "len=%d depth=%d %s" % (self.size(), self._depth, self.str_content())

class W_32BitDisplayBitmap(W_DisplayBitmap):
    repr_classname = "W_32BitDisplayBitmap"

    def force_words(self, start, stop):
        assert start > 0 and stop > 0 and self.size() >= stop and self.pixelbuffer_words >= stop and stop >= start
        pixbuf = rffi.ptradd(self.display().get_pixelbuffer(), start)
        realbuf = rffi.ptradd(self._real_depth_buffer, start)
        rffi.c_memcpy(
            rffi.cast(rffi.VOIDP, pixbuf),
            rffi.cast(rffi.VOIDP, realbuf),
            stop - start)


class W_16BitDisplayBitmap(W_DisplayBitmap):

    repr_classname = "W_16BitDisplayBitmap"

    def set_pixelbuffer_word(self, n, word):
        mask = 0b11111
        lsb = (r_uint(word) & r_uint(0xffff0000)) >> 16
        msb = (r_uint(word) & r_uint(0x0000ffff))

        if not system.IS_DARWIN:
            # Invert order of rgb-components
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
        self.pixelbuffer()[n] = r_uint(lsb | (msb << 16))

class W_8BitDisplayBitmap(W_DisplayBitmap):

    repr_classname = "W_8BitDisplayBitmap"

    def set_pixelbuffer_word(self, n, word):
        # Invert the byte-order.
        self.pixelbuffer()[n] = r_uint(
            (word >> 24) |
            ((word >> 8) & 0x0000ff00) |
            ((word << 8) & 0x00ff0000) |
            (word << 24)
        )

BITS = r_uint(32)
class W_MappingDisplayBitmap(W_DisplayBitmap):

    repr_classname = "W_MappingDisplayBitmap"
    _attrs_ = ['words_per_line', 'bits_in_last_word', 'pitch']
    _immutable_fields_ = ['words_per_line?', 'bits_in_last_word?', 'pitch?']

    def __init__(self, space, w_class, size, depth):
        assert depth in [1, 2, 4]
        W_DisplayBitmap.__init__(self, space, w_class, size, depth)

    def word_from_pixel(self, x, y):
        word = W_DisplayBitmap.word_from_pixel(self, x, y)
        if self._depth == 1:
            return word / 8
        elif self._depth == 2:
            return word / 4
        elif self._depth == 4:
            return word / 2
        else:
            assert False

    def take_over_display(self):
        pitch = r_uint(self.display().pitch) # The pitch is different from the width input to SDL!
        self.pitch = pitch
        self.bits_in_last_word = pitch % BITS
        self.words_per_line = r_uint((pitch - self.bits_in_last_word) / BITS)
        if self.bits_in_last_word > 0:
            self.words_per_line += 1
        W_DisplayBitmap.take_over_display(self)

    @jit.unroll_safe
    def set_pixelbuffer_word(self, n, word):
        n = r_uint(n)
        if ((n+1) % self.words_per_line) == 0 and self.bits_in_last_word > 0:
            # This is the last word on the line. A few bits are cut off.
            bits = self.bits_in_last_word
        else:
            bits = BITS

        word = r_uint(word)
        pos = self.compute_pos(n)
        buf = rffi.ptradd(self.display().get_plain_pixelbuffer(), pos)
        depth = r_uint(self._depth)
        rshift = BITS - depth
        for i in range(bits / depth):
            pixel = word >> rshift
            buf[i] = rffi.cast(rffi.CHAR, pixel)
            word <<= depth

    def compute_pos(self, n):
        word_on_line = n % self.words_per_line
        y = r_uint((n - word_on_line) / self.words_per_line)
        x = word_on_line * BITS / r_uint(self._depth)
        return y * self.pitch + x
