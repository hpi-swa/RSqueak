from rpython.rlib.rarithmetic import r_uint
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rlib.runicode import unicode_encode_utf_8
from rpython.rlib import jit

from rsdl import RSDL, RSDL_helper


# EventSensorConstants
RedButtonBit = 4
BlueButtonBit = 2
YellowButtonBit = 1

ShiftKeyBit = 1
CtrlKeyBit = 2
OptionKeyBit = 4
CommandKeyBit = 8

EventTypeNone = 0
EventTypeMouse = 1
EventTypeKeyboard = 2
EventTypeDragDropFiles = 3
EventTypeMenu = 4
EventTypeWindow = 5
EventTypeComplex = 6

EventKeyChar = 0
EventKeyDown = 1
EventKeyUp = 2

WindowEventMetricChange = 1
WindowEventClose = 2
WindowEventIconise = 3
WindowEventActivated = 4
WindowEventPaint = 5
WindowEventStinks = 6


class SDLDisplay(object):
    _attrs_ = ["screen", "width", "height", "depth", "surface", "has_surface",
               "mouse_position", "button", "key", "interrupt_key", "_defer_updates",
               "_deferred_event"]

    def __init__(self, title):
        assert RSDL.Init(RSDL.INIT_VIDEO) >= 0
        RSDL.WM_SetCaption(title, "RSqueakVM")
        RSDL.EnableUNICODE(1)
        SDLCursor.has_display = True
        self.has_surface = False
        self.mouse_position = [0, 0]
        self.interrupt_key = 15 << 8 # pushing all four meta keys, of which we support three...
        self.button = 0
        self.key = 0
        self._deferred_event = None
        self._defer_updates = False

    def set_video_mode(self, w, h, d):
        assert w > 0 and h > 0
        assert d in [1, 2, 4, 8, 16, 32]
        self.width = w
        self.height = h
        self.depth = d
        flags = RSDL.HWPALETTE | RSDL.RESIZABLE | RSDL.ASYNCBLIT | RSDL.DOUBLEBUF
        if d < 8:
            d = 8
        self.screen = RSDL.SetVideoMode(w, h, d, flags)
        if not self.screen:
            print "Could not open display at depth %d" % d
            raise RuntimeError
        elif d == 8:
            self.set_squeak_colormap(self.screen)

    def get_pixelbuffer(self):
        return rffi.cast(rffi.ULONGP, self.screen.c_pixels)

    def defer_updates(self, flag):
        self._defer_updates = flag

    def flip(self, force=False):
        if (not self._defer_updates) or force:
            RSDL.Flip(self.screen)

    def set_squeak_colormap(self, screen):
        # TODO: fix this up from the image
        colors = lltype.malloc(rffi.CArray(RSDL.ColorPtr.TO), 4, flavor='raw')
        colors[0].c_r = rffi.r_uchar(255)
        colors[0].c_g = rffi.r_uchar(255)
        colors[0].c_b = rffi.r_uchar(255)
        colors[1].c_r = rffi.r_uchar(0)
        colors[1].c_g = rffi.r_uchar(0)
        colors[1].c_b = rffi.r_uchar(0)
        colors[2].c_r = rffi.r_uchar(128)
        colors[2].c_g = rffi.r_uchar(128)
        colors[2].c_b = rffi.r_uchar(128)
        colors[3].c_r = rffi.r_uchar(255)
        colors[3].c_g = rffi.r_uchar(255)
        colors[3].c_b = rffi.r_uchar(255)
        RSDL.SetColors(self.screen, rffi.cast(RSDL.ColorPtr, colors), 0, 4)
        lltype.free(colors, flavor='raw')

    def handle_mouse_button(self, c_type, event):
        b = rffi.cast(RSDL.MouseButtonEventPtr, event)
        btn = rffi.getintfield(b, 'c_button')
        if btn == RSDL.BUTTON_RIGHT:
            btn = YellowButtonBit
        elif btn == RSDL.BUTTON_MIDDLE:
            btn = BlueButtonBit
        elif btn == RSDL.BUTTON_LEFT:
            btn = RedButtonBit

        if c_type == RSDL.MOUSEBUTTONDOWN:
            self.button |= btn
        else:
            self.button &= ~btn

    def handle_mouse_move(self, c_type, event):
        m = rffi.cast(RSDL.MouseMotionEventPtr, event)
        x = rffi.getintfield(m, "c_x")
        y = rffi.getintfield(m, "c_y")
        self.mouse_position = [x, y]

    def handle_keypress(self, c_type, event):
        self.key = 0
        p = rffi.cast(RSDL.KeyboardEventPtr, event)
        sym = rffi.getintfield(p.c_keysym, 'c_sym')
        char = rffi.getintfield(p.c_keysym, 'c_unicode')
        if sym == RSDL.K_DOWN:
            self.key = 31
        elif sym == RSDL.K_LEFT:
            self.key = 28
        elif sym == RSDL.K_RIGHT:
            self.key = 29
        elif sym == RSDL.K_UP:
            self.key = 30
        elif char != 0:
            chars = unicode_encode_utf_8(unichr(char), 1, "ignore")
            if len(chars) == 1:
                asciivalue = ord(chars[0])
                if asciivalue >= 32:
                    self.key = asciivalue
        if self.key == 0 and sym <= 255:
            self.key = sym
        interrupt = self.interrupt_key
        if (interrupt & 0xFF == self.key and interrupt >> 8 == self.get_modifier_mask(0)):
            raise KeyboardInterrupt

    def get_next_mouse_event(self, time):
        mods = self.get_modifier_mask(3)
        btn = self.button
        if btn == RedButtonBit:
            if mods & CtrlKeyBit:
                btn = BlueButtonBit
            elif mods & CommandKeyBit:
                btn = YellowButtonBit
        return [EventTypeMouse,
                time,
                int(self.mouse_position[0]),
                int(self.mouse_position[1]),
                btn,
                mods,
                0,
                0]

    def get_next_key_event(self, t, time):
        mods = self.get_modifier_mask(3)
        btn = self.button
        return [EventTypeKeyboard,
                time,
                self.key,
                t,
                mods,
                self.key,
                0,
                0]

    def get_next_event(self, time=0):
        if self._deferred_event:
            deferred = self._deferred_event
            self._deferred_event = None
            return deferred

        event = lltype.malloc(RSDL.Event, flavor="raw")
        try:
            if rffi.cast(lltype.Signed, RSDL.PollEvent(event)) == 1:
                c_type = rffi.getintfield(event, 'c_type')
                if c_type in [RSDL.MOUSEBUTTONDOWN, RSDL.MOUSEBUTTONUP]:
                    self.handle_mouse_button(c_type, event)
                    return self.get_next_mouse_event(time)
                elif c_type == RSDL.MOUSEMOTION:
                    self.handle_mouse_move(c_type, event)
                    return self.get_next_mouse_event(time)
                elif c_type == RSDL.KEYDOWN:
                    self.handle_keypress(c_type, event)
                    return self.get_next_key_event(EventKeyDown, time)
                elif c_type == RSDL.KEYUP:
                    self._deferred_event = self.get_next_key_event(EventKeyUp, time)
                    return self.get_next_key_event(EventKeyChar, time)
                elif c_type == RSDL.VIDEORESIZE:
                    self.screen = RSDL.GetVideoSurface()
                    self._deferred_event = [EventTypeWindow, time, WindowEventPaint,
                                            0, 0, int(self.screen.c_w), int(self.screen.c_h), 0]
                    return [EventTypeWindow, time, WindowEventMetricChange,
                            0, 0, int(self.screen.c_w), int(self.screen.c_h), 0]
                elif c_type == RSDL.VIDEOEXPOSE:
                    self._deferred_event = [EventTypeWindow, time, WindowEventPaint,
                                            0, 0, int(self.screen.c_w), int(self.screen.c_h), 0]
                    return [EventTypeWindow, time, WindowEventActivated, 0, 0, 0, 0, 0]
                elif c_type == RSDL.QUIT:
                    return [EventTypeWindow, time, WindowEventClose, 0, 0, 0, 0, 0]
        finally:
            lltype.free(event, flavor='raw')
        return [EventTypeNone, 0, 0, 0, 0, 0, 0, 0]

    # Old style event handling
    def pump_events(self):
        event = lltype.malloc(RSDL.Event, flavor="raw")
        try:
            if rffi.cast(lltype.Signed, RSDL.PollEvent(event)) == 1:
                c_type = rffi.getintfield(event, 'c_type')
                if c_type == RSDL.MOUSEBUTTONDOWN or c_type == RSDL.MOUSEBUTTONUP:
                    self.handle_mouse_button(c_type, event)
                    return
                elif c_type == RSDL.MOUSEMOTION:
                    self.handle_mouse_move(c_type, event)
                elif c_type == RSDL.KEYDOWN:
                    self.handle_keypress(c_type, event)
                    return
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
            modifier |= CtrlKeyBit
        if mod & RSDL.KMOD_SHIFT != 0:
            modifier |= ShiftKeyBit
        if mod & RSDL.KMOD_ALT != 0:
            modifier |= (OptionKeyBit | CommandKeyBit)
        return modifier << shift

    def mouse_point(self):
        self.pump_events()
        return self.mouse_position

    def mouse_button(self):
        self.pump_events()
        mod = self.get_modifier_mask(3)
        return self.button | mod

    def next_keycode(self):
        key = self.key
        self.key = 0
        return key | self.get_modifier_mask(8)

    def peek_keycode(self):
        self.pump_events()
        self.key |= self.get_modifier_mask(8)
        return self.key

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
