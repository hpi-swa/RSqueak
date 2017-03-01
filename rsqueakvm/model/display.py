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

    if depth in (1,2,4,8):
        w_display_bitmap = W_MappingDisplayBitmap(space, size, depth)
    elif depth in (16, 32):
        w_display_bitmap = W_DirectBitDisplayBitmap(space, size, depth)
    else:
        raise error.PrimitiveFailedError

    for idx in range(size):
        w_display_bitmap.setword(idx, w_obj.getword(idx))

    return w_display_bitmap


class W_DisplayBitmap(W_AbstractObjectWithIdentityHash):
    _attrs_ = ['is_display', '_squeak_pixel_buffer', '_realsize', '_display', '_depth']
    _immutable_fields_ = ['is_display?', '_squeak_pixel_buffer', '_realsize', '_display', '_depth']
    repr_classname = "W_DisplayBitmap"

    def __init__(self, space, size, depth):
        W_AbstractObjectWithIdentityHash.__init__(self)
        self._squeak_pixel_buffer = lltype.malloc(rffi.CArray(rffi.UINT), size, flavor='raw')
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
        return r_uint(self._squeak_pixel_buffer[n])

    def setword(self, n, word):
        self._squeak_pixel_buffer[n] = rffi.r_uint(word)

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

    def take_over_display(self):
        self.is_display = True

    def relinquish_display(self):
        self.is_display = False

    def flush_to_screen(self):
        self.force_rectange_to_screen(0, self.display().width, 0, self.display().height)
        self.display().render()

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

    def repr_content(self):
        return "len=%d depth=%d %s" % (self.size(), self._depth, self.str_content())


class W_DirectDisplayBitmap(W_DisplayBitmap):
    repr_classname = "W_DirectDisplayBitmap"

    def force_rectange_to_screen(self, left, right, top, bottom):
        if self.is_headless: return
        if self.is_display:
            self.display().flip(self._squeak_pixel_buffer, left, top, right, bottom)

    @rgc.must_be_light_finalizer
    def __del__(self):
        lltype.free(self._squeak_pixel_buffer, flavor='raw')


from rsqueakvm.display import BELOW_MINIMUM_DEPTH
class W_MappingDisplayBitmap(W_DisplayBitmap):
    repr_classname = "W_MappingDisplayBitmap"
    _attrs_ = ['_sdl_pixel_buffer', 'words_per_line', 'bits_in_last_word', 'pitch']
    _immutable_fields_ = ['_sdl_pixel_buffer', 'words_per_line?', 'bits_in_last_word?', 'pitch?']

    def __init__(self, space, size, depth):
        W_DisplayBitmap.__init__(self, space, size, depth)
        self.mapping_factor = BELOW_MINIMUM_DEPTH / self._depth
        self._sdl_pixel_buffer = lltype.malloc(rffi.CArray(rffi.UINT), size * self.mapping_factor, flavor='raw')

    @rgc.must_be_light_finalizer
    def __del__(self):
        lltype.free(self._squeak_pixel_buffer, flavor='raw')
        lltype.free(self._sdl_pixel_buffer, flavor='raw')

    def word_from_pixel(self, x, y):
        word = x + y * self.display().width
        return word / self.mapping_factor

    def take_over_display(self):
        W_DisplayBitmap.take_over_display(self)
        pitch = r_uint(self.display().pitch)  # The pitch may be different from the width input to SDL!
        self.pitch = pitch
        self.bits_in_last_word = pitch % BELOW_MINIMUM_DEPTH
        self.words_per_line = r_uint((pitch - self.bits_in_last_word) / constants.BITS_PER_WORD)
        if self.bits_in_last_word > 0:
            self.words_per_line += 1
        if self.is_headless(): return
        for i in range(self.size()):
            self.set_pixelbuffer_word(i, self.getword(i))

    @jit.unroll_safe
    def set_pixelbuffer_word(self, n, word):
        n = r_uint(n)
        if ((n+1) % self.words_per_line) == 0 and self.bits_in_last_word > 0:
            # This is the last word on the line. A few bits may be cut off.
            bits = self.bits_in_last_word
        else:
            bits = constants.BITS_PER_WORD
        buf = self._sdl_pixel_buffer
        word = r_uint(word)
        depth = r_uint(self._depth)
        shift = constants.BITS_PER_WORD - depth
        display_words_per_word =  BELOW_MINIMUM_DEPTH / depth
        pixelmask = ((r_uint(1) << depth) - 1) << shift
        table = PIXEL_LOOKUP_TABLE[depth - 1]
        assert table is not None
        for i in range(bits / depth):
            pixel = (word & pixelmask) >> (shift - i)
            buf[n * display_words_per_word + i] = rffi.r_uint(table[pixel])
            pixelmask >>= depth

    def force_rectange_to_screen(self, left, right, top, bottom):
        if self.is_headless(): return
        if self.is_display:
            start = max(self.word_from_pixel(left, top), 0)
            stop = min(self.word_from_pixel(right, bottom), self.size() - 1)
            if stop <= start:
                return
            for i in range(stop - start):
                self.set_pixelbuffer_word(i + start, self.getword(i + start))
            self.display().flip(self._sdl_pixel_buffer, left, top, right, bottom)


PIXEL_LOOKUP_1BIT = [0xffffffff, 0xff000000]
PIXEL_LOOKUP_2BIT = [0xff000000, 0xff848484, 0xffc6c6c6, 0xffffffff]
PIXEL_LOOKUP_4BIT = [
    0xff000000, 0xff000084, 0xff008400, 0xff008484,
    0xff840000, 0xff840084, 0xff848400, 0xff848484,
    0xffc6c6c6, 0xff0000ff, 0xff00ff00, 0xff00ffff,
    0xffff0000, 0xffff00ff, 0xffffff00, 0xffffffff
]
# String streamContents: [:s |
#       Color indexedColors
#               do: [:ea | s nextPutAll: '0x'; nextPutAll: ea printHtmlString]
#               separatedBy: [s nextPutAll: ', ']]
PIXEL_LOOKUP_8BIT = [
    0xFFFFFFFF, 0xFF000000, 0xFFFFFFFF, 0xFF7F7F7F, 0xFFFF0000, 0xFF00FF00,
    0xFF0000FF, 0xFF00FFFF, 0xFFFFFF00, 0xFFFF00FF, 0xFF1F1F1F, 0xFF3F3F3F,
    0xFF5F5F5F, 0xFF9F9F9F, 0xFFBFBFBF, 0xFFDFDFDF, 0xFF070707, 0xFF0F0F0F,
    0xFF171717, 0xFF272727, 0xFF2F2F2F, 0xFF373737, 0xFF474747, 0xFF4F4F4F,
    0xFF575757, 0xFF676767, 0xFF6F6F6F, 0xFF777777, 0xFF878787, 0xFF8F8F8F,
    0xFF979797, 0xFFA7A7A7, 0xFFAFAFAF, 0xFFB7B7B7, 0xFFC7C7C7, 0xFFCFCFCF,
    0xFFD7D7D7, 0xFFE7E7E7, 0xFFEFEFEF, 0xFFF7F7F7, 0xFF000000, 0xFF003200,
    0xFF006500, 0xFF009800, 0xFF00CB00, 0xFF00FF00, 0xFF000032, 0xFF003232,
    0xFF006532, 0xFF009832, 0xFF00CB32, 0xFF00FF32, 0xFF000065, 0xFF003265,
    0xFF006565, 0xFF009865, 0xFF00CB65, 0xFF00FF65, 0xFF000098, 0xFF003298,
    0xFF006598, 0xFF009898, 0xFF00CB98, 0xFF00FF98, 0xFF0000CB, 0xFF0032CB,
    0xFF0065CB, 0xFF0098CB, 0xFF00CBCB, 0xFF00FFCB, 0xFF0000FF, 0xFF0032FF,
    0xFF0065FF, 0xFF0098FF, 0xFF00CBFF, 0xFF00FFFF, 0xFF320000, 0xFF323200,
    0xFF326500, 0xFF329800, 0xFF32CB00, 0xFF32FF00, 0xFF320032, 0xFF323232,
    0xFF326532, 0xFF329832, 0xFF32CB32, 0xFF32FF32, 0xFF320065, 0xFF323265,
    0xFF326565, 0xFF329865, 0xFF32CB65, 0xFF32FF65, 0xFF320098, 0xFF323298,
    0xFF326598, 0xFF329898, 0xFF32CB98, 0xFF32FF98, 0xFF3200CB, 0xFF3232CB,
    0xFF3265CB, 0xFF3298CB, 0xFF32CBCB, 0xFF32FFCB, 0xFF3200FF, 0xFF3232FF,
    0xFF3265FF, 0xFF3298FF, 0xFF32CBFF, 0xFF32FFFF, 0xFF650000, 0xFF653200,
    0xFF656500, 0xFF659800, 0xFF65CB00, 0xFF65FF00, 0xFF650032, 0xFF653232,
    0xFF656532, 0xFF659832, 0xFF65CB32, 0xFF65FF32, 0xFF650065, 0xFF653265,
    0xFF656565, 0xFF659865, 0xFF65CB65, 0xFF65FF65, 0xFF650098, 0xFF653298,
    0xFF656598, 0xFF659898, 0xFF65CB98, 0xFF65FF98, 0xFF6500CB, 0xFF6532CB,
    0xFF6565CB, 0xFF6598CB, 0xFF65CBCB, 0xFF65FFCB, 0xFF6500FF, 0xFF6532FF,
    0xFF6565FF, 0xFF6598FF, 0xFF65CBFF, 0xFF65FFFF, 0xFF980000, 0xFF983200,
    0xFF986500, 0xFF989800, 0xFF98CB00, 0xFF98FF00, 0xFF980032, 0xFF983232,
    0xFF986532, 0xFF989832, 0xFF98CB32, 0xFF98FF32, 0xFF980065, 0xFF983265,
    0xFF986565, 0xFF989865, 0xFF98CB65, 0xFF98FF65, 0xFF980098, 0xFF983298,
    0xFF986598, 0xFF989898, 0xFF98CB98, 0xFF98FF98, 0xFF9800CB, 0xFF9832CB,
    0xFF9865CB, 0xFF9898CB, 0xFF98CBCB, 0xFF98FFCB, 0xFF9800FF, 0xFF9832FF,
    0xFF9865FF, 0xFF9898FF, 0xFF98CBFF, 0xFF98FFFF, 0xFFCB0000, 0xFFCB3200,
    0xFFCB6500, 0xFFCB9800, 0xFFCBCB00, 0xFFCBFF00, 0xFFCB0032, 0xFFCB3232,
    0xFFCB6532, 0xFFCB9832, 0xFFCBCB32, 0xFFCBFF32, 0xFFCB0065, 0xFFCB3265,
    0xFFCB6565, 0xFFCB9865, 0xFFCBCB65, 0xFFCBFF65, 0xFFCB0098, 0xFFCB3298,
    0xFFCB6598, 0xFFCB9898, 0xFFCBCB98, 0xFFCBFF98, 0xFFCB00CB, 0xFFCB32CB,
    0xFFCB65CB, 0xFFCB98CB, 0xFFCBCBCB, 0xFFCBFFCB, 0xFFCB00FF, 0xFFCB32FF,
    0xFFCB65FF, 0xFFCB98FF, 0xFFCBCBFF, 0xFFCBFFFF, 0xFFFF0000, 0xFFFF3200,
    0xFFFF6500, 0xFFFF9800, 0xFFFFCB00, 0xFFFFFF00, 0xFFFF0032, 0xFFFF3232,
    0xFFFF6532, 0xFFFF9832, 0xFFFFCB32, 0xFFFFFF32, 0xFFFF0065, 0xFFFF3265,
    0xFFFF6565, 0xFFFF9865, 0xFFFFCB65, 0xFFFFFF65, 0xFFFF0098, 0xFFFF3298,
    0xFFFF6598, 0xFFFF9898, 0xFFFFCB98, 0xFFFFFF98, 0xFFFF00CB, 0xFFFF32CB,
    0xFFFF65CB, 0xFFFF98CB, 0xFFFFCBCB, 0xFFFFFFCB, 0xFFFF00FF, 0xFFFF32FF,
    0xFFFF65FF, 0xFFFF98FF, 0xFFFFCBFF, 0xFFFFFFFF
]
PIXEL_LOOKUP_TABLE = [
    PIXEL_LOOKUP_1BIT,
    PIXEL_LOOKUP_2BIT,
    None,
    PIXEL_LOOKUP_4BIT,
    None,
    None,
    None,
    PIXEL_LOOKUP_8BIT
]
