from rpython.rlib.rarithmetic import r_uint
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rlib.runicode import unicode_encode_utf_8
from rpython.rlib import jit

from rsdl import RSDL, RSDL_helper


MOUSE_BTN_RIGHT = 1
MOUSE_BTN_MIDDLE = 2
MOUSE_BTN_LEFT = 4
MOD_SHIFT  = 1
MOD_CONTROL = 2
MOD_ALT = 16

class SDLDisplay(object):
    _attrs_ = ["screen", "width", "height", "depth", "surface", "has_surface",
               "mouse_position", "button", "key", "interrupt_key"]

    def __init__(self, title):
        assert RSDL.Init(RSDL.INIT_VIDEO) >= 0
        RSDL.WM_SetCaption(title, "RSqueakVM")
        RSDL.EnableUNICODE(1)
        SDLCursor.has_display = True
        self.has_surface = False
        self.mouse_position = [0, 0]
        self.button = 0
        self.key = 0

    def set_video_mode(self, w, h, d):
        assert w > 0 and h > 0
        assert d in [1, 2, 4, 8, 16, 32]
        self.width = w
        self.height = h
        self.depth = d
        self.screen = RSDL.SetVideoMode(w, h, 32, 0)
        assert self.screen

    def get_pixelbuffer(self):
        return rffi.cast(rffi.ULONGP, self.screen.c_pixels)

    def flip(self):
        RSDL.Flip(self.screen)

    def get_next_event(self):
        event = lltype.malloc(RSDL.Event, flavor="raw")
        ok = 1
        try:
            while ok == 1:
                ok = rffi.cast(lltype.Signed, RSDL.PollEvent(event))
                if ok == 1:
                    c_type = rffi.getintfield(event, 'c_type')
                    if c_type == RSDL.MOUSEBUTTONDOWN or c_type == RSDL.MOUSEBUTTONUP:
                        b = rffi.cast(RSDL.MouseButtonEventPtr, event)
                        btn = rffi.getintfield(b, 'c_button')
                        if btn == RSDL.BUTTON_RIGHT:
                            btn = MOUSE_BTN_RIGHT
                        elif btn == RSDL.BUTTON_MIDDLE:
                            btn = MOUSE_BTN_MIDDLE
                        elif btn == RSDL.BUTTON_LEFT:
                            btn = MOUSE_BTN_LEFT

                        if c_type == RSDL.MOUSEBUTTONDOWN:
                            self.button |= btn
                        else:
                            self.button &= ~btn
                    elif c_type == RSDL.MOUSEMOTION:
                        m = rffi.cast(RSDL.MouseMotionEventPtr, event)
                        x = rffi.getintfield(m, "c_x")
                        y = rffi.getintfield(m, "c_y")
                        self.mouse_position = [x, y]
                    elif c_type == RSDL.KEYUP or c_type == RSDL.KEYDOWN:
                        p = rffi.cast(RSDL.KeyboardEventPtr, event)
                        char = rffi.getintfield(p.c_keysym, 'c_unicode')
                        if char != 0:
                            chars = unicode_encode_utf_8(unichr(char), 1, "ignore")
                            if len(chars) == 1:
                                if c_type == RSDL.KEYDOWN:
                                    self.key = ord(chars[0])
                                    interrupt = self.interrupt_key
                                    if (interrupt & 0xFF == self.key and
                                        interrupt >> 8 == self.get_modifier_mask(0)):
                                            raise KeyboardInterrupt
                                else:
                                    pass # XXX: Todo?
                    elif c_type == RSDL.QUIT:
                        from spyvm.error import Exit
                        raise Exit("Window closed..")
        finally:
            lltype.free(event, flavor='raw')

    def get_modifier_mask(self, shift):
        RSDL.PumpEvents()
        mod = RSDL.GetModState()
        modifier = 0
        if mod & RSDL.KMOD_CTRL != 0:
            modifier |= MOD_CONTROL
        if mod & RSDL.KMOD_SHIFT != 0:
            modifier |= MOD_SHIFT
        if mod & RSDL.KMOD_ALT != 0:
            modifier |= MOD_ALT
        return modifier << shift

    def mouse_point(self):
        self.get_next_event()
        return self.mouse_position

    def mouse_button(self):
        self.get_next_event()
        return self.button | self.get_modifier_mask(3)

    def next_keycode(self):
        key = self.key
        self.key = 0
        return key | self.get_modifier_mask(8)

    def peek_keycode(self):
        self.get_next_event()
        return self.key | self.get_modifier_mask(8)

    def set_interrupt_key(self, space, encoded_key):
        self.interrupt_key = encoded_key


class SDLCursorClass(object):
    _attrs_ = ["cursor", "has_cursor", "has_display"]

    instance = None

    def __init__(self):
        self.cursor = lltype.nullptr(RSDL.CursorPtr.TO)
        self.has_cursor = False
        self.has_display = False

    def set(self, data_words, w, h, x, y, mask_words=None):
        if not self.has_display:
            return
        if self.has_cursor:
            RSDL.FreeCursor(self.cursor)
        data = self.words_to_bytes(len(data_words) * 4, data_words)
        try:
            mask = self.words_to_bytes(len(data_words) * 4, mask_words)
            try:
                self.cursor = RSDL.CreateCursor(data, mask, w * 2, h, x, y)
                self.has_cursor = True
                RSDL.SetCursor(self.cursor)
            finally:
                lltype.free(mask, flavor="raw")
        finally:
            lltype.free(data, flavor="raw")

    def words_to_bytes(self, bytenum, words):
        bytes = lltype.malloc(RSDL.Uint8P.TO, bytenum, flavor="raw")
        if words:
            for pos in range(bytenum / 4):
                word = words[pos]
                bytes[pos * 4] = rffi.r_uchar((word >> 24) & 0xff)
                bytes[pos * 4 + 1] = rffi.r_uchar((word >> 16) & 0xff)
                bytes[pos * 4 + 2] = rffi.r_uchar((word >> 8) & 0xff)
                bytes[pos * 4 + 3] = rffi.r_uchar(word & 0xff)
        else:
            for idx in range(bytenum):
                bytes[idx] = rffi.r_uchar(0)
        return bytes

SDLCursor = SDLCursorClass()
