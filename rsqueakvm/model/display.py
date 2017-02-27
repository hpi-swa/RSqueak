from rsqueakvm import constants, error
from rsqueakvm.model.base import W_AbstractObjectWithIdentityHash
from rsqueakvm.model.variable import W_WordsObject
from rsqueakvm.util import system

from rpython.rlib import jit, rgc
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rlib.rarithmetic import r_uint


def from_words_object(w_obj, form):
    depth = form.depth()
    space = form.space
    size = w_obj.size()
    w_class = w_obj.getclass(space)
    if not w_class.is_same_object(space.w_Bitmap):
        raise error.WrappingError

    if depth < 8:
        w_display_bitmap = W_MappingDisplayBitmap(space, size, depth)
    elif depth == 8:
        w_display_bitmap = W_8BitDisplayBitmap(space, size, depth)
    elif depth == 16:
        w_display_bitmap = W_16BitDisplayBitmap(space, size, depth)
    else:
        assert depth == 32
        w_display_bitmap = W_32BitDisplayBitmap(space, size, depth)

    for idx in range(size):
        w_display_bitmap.setword(idx, w_obj.getword(idx))

    return w_display_bitmap


class W_DisplayBitmap(W_AbstractObjectWithIdentityHash):
    _attrs_ = ['pixelbuffer_words', '_real_depth_buffer', '_realsize', '_display', '_depth']
    _immutable_fields_ = ['pixelbuffer_words?', '_real_depth_buffer', '_realsize', '_display', '_depth']
    repr_classname = "W_DisplayBitmap"

    def __init__(self, space, size, depth):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self._real_depth_buffer = lltype.malloc(rffi.CArray(rffi.UINT), size, flavor='raw')
        self._realsize = size
        self._depth = depth
        self._display = space.display()
        self.relinquish_display()

    def getclass(self, space):
        return space.w_Bitmap

    def guess_classname(self):
        return "Bitmap"

    # === Object access

    def at0(self, space, index0):
        self = jit.promote(self)
        val = self.getword(index0)
        return space.wrap_int(r_uint(val))

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
        return r_uint(self._real_depth_buffer[n])

    def setword(self, n, word):
        self._real_depth_buffer[n] = rffi.r_uint(word)

    def setwords(self, lst):
        for i in range(self.size()):
            self.setword(i, lst[i])

    def size(self):
        return self._realsize

    # === Graphics

    def display(self):
        return jit.promote(self._display)

    def is_headless(self):
        return self._display.is_headless()

    def pixelbuffer(self):
        return self.display().get_pixelbuffer()

    def set_pixelbuffer_word(self, n, word):
        self.pixelbuffer()[n] = rffi.r_uint(word)

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
        self.display().render()

    def word_from_pixel(self, x, y):
        return (x - 1 + (y - 1) * self.display().width) / self.pixel_per_word()

    def force_rectange_to_screen(self, left, right, top, bottom):
        if self.pixelbuffer_words > 0:
            start = max(self.word_from_pixel(left, top), 0)
            stop = min(self.word_from_pixel(right, bottom), self.size() - 1)
            if stop <= start:
                return
            self.force_words(start, stop)
            self.display().flip()

    def force_words(self, start, stop):
        if self.is_headless(): return
        for i in range(stop - start):
            self.set_pixelbuffer_word(i + start, self.getword(i + start))

    def update_from_buffer(self):
        if self.is_headless(): return
        if self.pixelbuffer_words > 0:
            for i in range(self.size()):
                self.set_pixelbuffer_word(i, self.getword(i))

    # === Misc

    def invariant(self):
        return False

    def clone(self, space):
        w_result = W_WordsObject(space, self.getclass(space), self.size())
        for n in range(self.size()):
            w_result.setword(n, self.getword(n))
        return w_result

    def is_array_object(self):
        return True

    def can_become(self, w_other):
        # TODO - implement _become() for this class. Impossible due to _immutable_fields_?
        return False

    @rgc.must_be_light_finalizer
    def __del__(self):
        lltype.free(self._real_depth_buffer, flavor='raw')

    def repr_content(self):
        return "len=%d depth=%d %s" % (self.size(), self._depth, self.str_content())

class W_32BitDisplayBitmap(W_DisplayBitmap):
    repr_classname = "W_32BitDisplayBitmap"

    def take_over_display(self):
        if self.pixelbuffer_words == 0:
            W_DisplayBitmap.take_over_display(self)
            assert self.pixelbuffer_words == self.size()
            lltype.free(self._real_depth_buffer, flavor='raw')
            self._real_depth_buffer = self.pixelbuffer()

    def relinquish_display(self):
        W_DisplayBitmap.relinquish_display(self)
        assert self.pixelbuffer_words == 0
        self._real_depth_buffer = lltype.malloc(rffi.CArray(rffi.UINT), self.size(), flavor='raw')
        self._copy_pixelbuffer_to_own()

    @rgc.must_be_light_finalizer
    def __del__(self):
        if self.pixelbuffer_words == 0:
            lltype.free(self._real_depth_buffer, flavor='raw')

    def _copy_pixelbuffer_to_own(self):
        for i in range(self.size()):
            self.setword(i, self.pixelbuffer()[i])

    def force_rectange_to_screen(self, left, right, top, bottom):
        if self.pixelbuffer_words > 0:
            self.display().flip()



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
        self.pixelbuffer()[n] = rffi.r_uint(lsb | (msb << 16))

class W_8BitDisplayBitmap(W_DisplayBitmap):

    repr_classname = "W_8BitDisplayBitmap"

    def set_pixelbuffer_word(self, n, word):
        # Invert the byte-order.
        self.pixelbuffer()[n] = rffi.r_uint(
            (word >> 24) |
            ((word >> 8) & 0x0000ff00) |
            ((word << 8) & 0x00ff0000) |
            (word << 24)
        )


class W_MappingDisplayBitmap(W_DisplayBitmap):

    repr_classname = "W_MappingDisplayBitmap"
    _attrs_ = ['words_per_line', 'bits_in_last_word', 'pitch']
    _immutable_fields_ = ['words_per_line?', 'bits_in_last_word?', 'pitch?']

    def __init__(self, space, size, depth):
        assert depth in [1, 2, 4]
        W_DisplayBitmap.__init__(self, space, size, depth)

    def word_from_pixel(self, x, y):
        word = W_DisplayBitmap.word_from_pixel(self, x, y)
        if self._depth == 1:
            return word / 32
        elif self._depth == 2:
            return word / 16
        elif self._depth == 4:
            return word / 8
        else:
            assert False

    def display_words_per_word(self):
        return self.display().depth / self._depth

    def take_over_display(self):
        pitch = r_uint(self.display().pitch)  # The pitch may be different from the width input to SDL!
        self.pitch = pitch
        self.bits_in_last_word = pitch % 32
        self.words_per_line = r_uint((pitch - self.bits_in_last_word) / 32)
        if self.bits_in_last_word > 0:
            self.words_per_line += 1
        W_DisplayBitmap.take_over_display(self)

    @jit.unroll_safe
    def set_pixelbuffer_word(self, n, word):
        n = r_uint(n)
        if ((n+1) % self.words_per_line) == 0 and self.bits_in_last_word > 0:
            # This is the last word on the line. A few bits may be cut off.
            bits = self.bits_in_last_word
        else:
            bits = 32
        buf = self.pixelbuffer()
        word = r_uint(word)
        depth = r_uint(self._depth)
        shift = 32 - depth
        display_words_per_word = 32 / depth
        pixelmask = ((r_uint(1) << depth) - 1) << shift
        table = PIXEL_LOOKUP_TABLE[depth - 1]
        assert table is not None
        for i in range(bits / depth):
            pixel = (word & pixelmask) >> (shift - i)
            buf[n * display_words_per_word + i] = rffi.r_uint(table[pixel])
            pixelmask >>= depth


PIXEL_LOOKUP_1BIT = [0xffffffff, 0xff000000]
PIXEL_LOOKUP_2BIT = [0xff000000, 0xff848484, 0xffc6c6c6, 0xffffffff]
PIXEL_LOOKUP_4BIT = [
    0xff000000, 0xff000084, 0xff008400, 0xff008484,
    0xff840000, 0xff840084, 0xff848400, 0xff848484,
    0xffc6c6c6, 0xff0000ff, 0xff00ff00, 0xff00ffff,
    0xffff0000, 0xffff00ff, 0xffffff00, 0xffffffff]
PIXEL_LOOKUP_TABLE = [PIXEL_LOOKUP_1BIT, PIXEL_LOOKUP_2BIT, None, PIXEL_LOOKUP_4BIT]
