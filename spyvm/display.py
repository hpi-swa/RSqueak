from rpython.rlib.rarithmetic import r_uint
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rlib.runicode import unicode_encode_utf_8
from rpython.rlib import jit

from rsdl import RSDL, RSDL_helper


class SDLEventQueue(object):
    def __init__(self, default, maxlen=10):
        assert default
        self.default = default
        self.ary = []
        self.maxlen = 10

    def push(self, event):
        if len(self.ary) == self.maxlen:
            self.ary.pop(0)
        self.ary.append(event)

    def shift(self):
        if not self.ary:
            self.ary += self.default
        return self.ary.pop(0)

    def peek(self):
        if self.ary:
            return self.ary[0]
        else:
            return self.default[0]

    def size(self):
        if not self.ary:
            return len(self.default)
        else:
            return len(self.ary)


class SDLDisplay(object):
    _attrs_ = ["screen", "width", "height", "depth", "surface", "has_surface",
               "last_mouse_position", "mouse_downs", "mouse_ups", "key_ups", "key_downs"]

    def __init__(self, title):
        assert RSDL.Init(RSDL.INIT_VIDEO) >= 0
        RSDL.WM_SetCaption(title, "RSqueakVM")
        RSDL.EnableUNICODE(1)
        self.has_surface = False
        self.last_mouse_position = [0, 0]
        self.mouse_downs = SDLEventQueue([0])
        self.mouse_ups = SDLEventQueue([0])
        self.key_ups = SDLEventQueue([0])
        self.key_downs = SDLEventQueue([0])

    def set_video_mode(self, w, h, d):
        assert w > 0 and h > 0
        assert d in [1, 2, 4, 8, 16, 32]
        self.width = w
        self.height = h
        self.depth = d
        self.screen = RSDL.SetVideoMode(w, h, 32, 0)
        assert self.screen

    def get_pixelbuffer(self):
        return self.screen.c_pixels

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
                        if btn == RSDL.BUTTON_LEFT:
                            btn = 1
                        elif btn == RSDL.BUTTON_MIDDLE:
                            btn = 2
                        elif btn == RSDL.BUTTON_RIGHT:
                            btn = 4

                        if c_type == RSDL.MOUSEBUTTONDOWN:
                            self.mouse_downs.push(btn)
                        else:
                            self.mouse_ups.push(btn)
                    elif c_type == RSDL.MOUSEMOTION:
                        m = rffi.cast(RSDL.MouseMotionEventPtr, event)
                        x = rffi.getintfield(m, "c_x")
                        y = rffi.getintfield(m, "c_y")
                        self.last_mouse_position = [x, y]
                    elif c_type == RSDL.KEYUP or c_type == RSDL.KEYDOWN:
                        p = rffi.cast(RSDL.KeyboardEventPtr, event)
                        char = rffi.getintfield(p.c_keysym, 'c_unicode')
                        if char != 0:
                            for c in unicode_encode_utf_8(unichr(char), 1, "ignore"):
                                if c_type == RSDL.KEYUP:
                                    self.key_ups.push(ord(c))
                                else:
                                    self.key_downs.push(ord(c))
        finally:
            lltype.free(event, flavor='raw')

    def mouse_point(self):
        self.get_next_event()
        return self.last_mouse_position

    def mouse_button(self):
        self.get_next_event()
        return self.mouse_ups.shift()

    def next_keycode(self):
        self.get_next_event()
        return self.key_downs.shift()

    def peek_keycode(self):
        self.get_next_event()
        return self.key_downs.peek()
