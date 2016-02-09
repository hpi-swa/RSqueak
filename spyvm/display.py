from rpython.rlib.rarithmetic import r_uint, intmask
from rpython.rtyper.lltypesystem import lltype, rffi
from rpython.rlib.runicode import unicode_encode_utf_8
from rpython.rlib import jit

from rsdl import RSDL, RSDL_helper
import key_constants


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

MINIMUM_DEPTH = 8

class SDLDisplay(object):
    _attrs_ = ["window", "title", "renderer", "screen_texture",
               "width", "height", "depth", "screen_surface", "has_surface",
               "mouse_position", "button", "key", "interrupt_key", "_defer_updates",
               "_deferred_events", "bpp", "pitch"]

    def __init__(self, title):
        self._init_sdl()
        self.title = title
        SDLCursor.has_display = True
        self.window = lltype.nullptr(RSDL.WindowPtr.TO)
        self.renderer = lltype.nullptr(RSDL.RendererPtr.TO)
        self.screen_texture = lltype.nullptr(RSDL.TexturePtr.TO)
        self.screen_surface = lltype.nullptr(RSDL.Surface)
        self.has_surface = False
        self.mouse_position = [0, 0]
        self.interrupt_key = 15 << 8 # pushing all four meta keys, of which we support three...
        self.button = 0
        self.key = 0
        self.width = 0
        self.height = 0
        self.depth = 32
        self._deferred_events = []
        self._defer_updates = False

    def _init_sdl(self):
        from rpython.rlib.objectmodel import we_are_translated
        if we_are_translated():
            assert RSDL.Init(RSDL.INIT_VIDEO) >= 0
        else:
            if RSDL.Init(RSDL.INIT_VIDEO) < 0:
                print RSDL.GetError()
                assert False

    def close(self):
        RSDL.Quit()

    def create_window_and_renderer(self, x, y, width, height):
        self.window = RSDL.CreateWindow(self.title, x, y, width, height,
                RSDL.WINDOW_RESIZABLE)
        self.renderer = RSDL.CreateRenderer(self.window, -1,
                RSDL.RENDERER_ACCELERATED)

    def set_video_mode(self, w, h, d):
        if not (w > 0 and h > 0):
            return
        assert d in [1, 2, 4, 8, 16, 32]
        if d < MINIMUM_DEPTH:
            d = MINIMUM_DEPTH
        self.width = w
        self.height = h
        self.depth = d
        if self.window == lltype.nullptr(RSDL.WindowPtr.TO):
            self.create_window_and_renderer(x=RSDL.WINDOWPOS_UNDEFINED,
                    y=RSDL.WINDOWPOS_UNDEFINED,
                    width=w,
                    height=h)
        if self.screen_texture != lltype.nullptr(RSDL.TexturePtr.TO):
            RSDL.DestroyTexture(self.screen_texture)
        if self.screen_surface != lltype.nullptr(RSDL.Surface):
            RSDL.FreeSurface(self.screen_surface)
        self.has_surface = True
        self.screen_texture = RSDL.CreateTexture(self.renderer,
                RSDL.PIXELFORMAT_ARGB8888, RSDL.TEXTUREACCESS_STREAMING,
                w, h)
        if not self.screen_texture:
            print "Could not create screen texture"
            raise RuntimeError(RSDL.GetError())
        self.screen_surface = RSDL.CreateRGBSurface(0, w, h, d, 0, 0, 0, 0)
        assert self.screen_surface, RSDL.GetError()
        self.bpp = r_uint(self.screen_surface.c_format.c_BytesPerPixel)
        if d == MINIMUM_DEPTH:
            self.set_squeak_colormap(self.screen_surface)
        self.pitch = w * self.bpp

    def get_pixelbuffer(self):
        return jit.promote(rffi.cast(RSDL.Uint32P, self.get_plain_pixelbuffer()))

    def get_plain_pixelbuffer(self):
        return self.screen_surface.c_pixels

    def defer_updates(self, flag):
        self._defer_updates = flag

    def flip(self, force=False):
        if self._defer_updates and not force:
            return
        assert RSDL.UpdateTexture(self.screen_texture, lltype.nullptr(RSDL.Rect),
                self.screen_surface.c_pixels, self.screen_surface.c_pitch) \
                        == 0, RSDL.GetError()
        assert RSDL.RenderCopy(self.renderer, self.screen_texture, lltype.nullptr(RSDL.Rect), lltype.nullptr(RSDL.Rect)) \
                == 0, RSDL.GetError()
        RSDL.RenderPresent(self.renderer)

    def set_squeak_colormap(self, surface):
        # TODO: fix this up from the image
        colors = lltype.malloc(rffi.CArray(RSDL.Color), 4, flavor='raw')
        colors[0].c_r = rffi.r_uchar(255)
        colors[0].c_g = rffi.r_uchar(255)
        colors[0].c_b = rffi.r_uchar(255)
        colors[0].c_a = rffi.r_uchar(255)
        colors[1].c_r = rffi.r_uchar(0)
        colors[1].c_g = rffi.r_uchar(0)
        colors[1].c_b = rffi.r_uchar(0)
        colors[1].c_a = rffi.r_uchar(255)
        colors[2].c_r = rffi.r_uchar(128)
        colors[2].c_g = rffi.r_uchar(128)
        colors[2].c_b = rffi.r_uchar(128)
        colors[2].c_a = rffi.r_uchar(255)
        colors[3].c_r = rffi.r_uchar(255)
        colors[3].c_g = rffi.r_uchar(255)
        colors[3].c_b = rffi.r_uchar(255)
        colors[3].c_a = rffi.r_uchar(255)
        RSDL.SetPaletteColors(surface.c_format.c_palette, rffi.cast(RSDL.ColorPtr, colors), 0, 4)
        lltype.free(colors, flavor='raw')

    def handle_mouse_button(self, c_type, event):
        b = rffi.cast(RSDL.MouseButtonEventPtr, event)
        btn = b.c_button
        if btn == RSDL.BUTTON_RIGHT:
            btn = YellowButtonBit
        elif btn == RSDL.BUTTON_MIDDLE:
            btn = BlueButtonBit
        elif btn == RSDL.BUTTON_LEFT:
            btn = RedButtonBit

        if c_type == RSDL.MOUSEBUTTONDOWN:
            self.button |= intmask(btn)
        else:
            self.button &= ~intmask(btn)

    def handle_mouse_move(self, c_type, event):
        m = rffi.cast(RSDL.MouseMotionEventPtr, event)
        x = m.c_x
        y = m.c_y
        self.mouse_position = [x, y]

    def handle_keyboard_event(self, c_type, event):
        self.key = 0
        p = rffi.cast(RSDL.KeyboardEventPtr, event)
        sym = p.c_keysym.c_sym
        if sym == RSDL.K_DOWN:
            self.key = key_constants.DOWN
        elif sym == RSDL.K_LEFT:
            self.key = key_constants.LEFT
        elif sym == RSDL.K_RIGHT:
            self.key = key_constants.RIGHT
        elif sym == RSDL.K_UP:
            self.key = key_constants.UP
        elif sym == RSDL.K_HOME:
            self.key = key_constants.HOME
        elif sym == RSDL.K_END:
            self.key = key_constants.END
        elif sym == RSDL.K_INSERT:
            self.key = key_constants.INSERT
        elif sym == RSDL.K_PAGEUP:
            self.key = key_constants.PAGEUP
        elif sym == RSDL.K_PAGEDOWN:
            self.key = key_constants.PAGEDOWN
        elif sym == RSDL.K_LSHIFT or sym == RSDL.K_RSHIFT:
            self.key = key_constants.SHIFT
        elif sym == RSDL.K_LCTRL or sym == RSDL.K_RCTRL:
            self.key = key_constants.CTRL
        elif sym == RSDL.K_LALT or sym == RSDL.K_RALT:
            self.key = key_constants.COMMAND
        elif sym == RSDL.K_PAUSE:
            self.key = key_constants.BREAK
        elif sym == RSDL.K_CAPSLOCK:
            self.key = key_constants.CAPSLOCK
        elif sym == RSDL.K_NUMLOCKCLEAR:
            self.key = key_constants.NUMLOCK
        elif sym == RSDL.K_SCROLLLOCK:
            self.key = key_constants.SCROLLLOCK
        elif sym == RSDL.K_PRINTSCREEN:
            self.key = key_constants.PRINT
        else:
            self.key = rffi.cast(rffi.INT, sym) # use SDL's keycode
            # this is the lowercase ascii-value for the most common keys
        # elif char != 0:
        #     chars = unicode_encode_utf_8(unichr(char), 1, "ignore")
        #     if len(chars) == 1:
        #         asciivalue = ord(chars[0])
        #         if asciivalue >= 32:
        #             self.key = asciivalue
        if self.key == 0 and sym <= 255:
            self.key = sym
        interrupt = self.interrupt_key
        if (interrupt & 0xFF == self.key and interrupt >> 8 == self.get_modifier_mask(0)):
            raise KeyboardInterrupt

    def handle_textinput_event(self, event):
        textinput = rffi.cast(RSDL.TextInputEventPtr, event)
        self.key = ord(rffi.charp2str(rffi.cast(rffi.CCHARP, textinput.c_text))[0])
        # XXX: textinput.c_text could contain multiple characters
        #      so probably multiple Squeak events must be emitted
        #      moreover, this is UTF-8 so umlauts etc. have to be decoded

    def handle_windowevent(self, c_type, event):
        window_event = rffi.cast(RSDL.WindowEventPtr, event)
        if r_uint(window_event.c_event) == RSDL.WINDOWEVENT_RESIZED:
            self.set_video_mode(w=window_event.c_data1,
                    h=window_event.c_data2,
                    d=self.depth)

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

    def get_next_key_event(self, key_event_type, time):
        mods = self.get_modifier_mask(0)
        btn = self.button
        return [EventTypeKeyboard,
                time,
                self.key,
                key_event_type,
                mods,
                self.key,
                0,
                0]

    def get_next_event(self, time=0):
        if len(self._deferred_events) > 0:
            deferred = self._deferred_events.pop()
            return deferred

        event = lltype.malloc(RSDL.Event, flavor="raw")
        try:
            if RSDL.PollEvent(event) == 1:
                event_type = event.c_type
                if event_type in [RSDL.MOUSEBUTTONDOWN, RSDL.MOUSEBUTTONUP]:
                    self.handle_mouse_button(event_type, event)
                    return self.get_next_mouse_event(time)
                elif event_type == RSDL.MOUSEMOTION:
                    self.handle_mouse_move(event_type, event)
                    return self.get_next_mouse_event(time)
                elif event_type == RSDL.KEYDOWN:
                    self.handle_keyboard_event(event_type, event)
                    if not self.is_modifier_key(self.key) and (
                            self.is_control_key(self.key)
                            or RSDL.GetModState() & ~RSDL.KMOD_SHIFT != 0):
                        # no TEXTINPUT event for this key will follow
                        # but Squeak needs a KeyStroke anyway
                        self._deferred_events.append(
                                self.get_next_key_event(EventKeyChar, time))
                    self.fix_key_code_case()
                    return self.get_next_key_event(EventKeyDown, time)
                elif event_type == RSDL.TEXTINPUT:
                    self.handle_textinput_event(event)
                    return self.get_next_key_event(EventKeyChar, time)
                elif event_type == RSDL.KEYUP:
                    self.handle_keyboard_event(event_type, event)
                    self.fix_key_code_case()
                    return self.get_next_key_event(EventKeyUp, time)
                elif event_type == RSDL.WINDOWEVENT:
                    self.handle_windowevent(event_type, event)
                #     self.screen = RSDL.GetVideoSurface()
                #     self._deferred_events.append([EventTypeWindow, time, WindowEventPaint,
                #                             0, 0, int(self.screen.c_w), int(self.screen.c_h), 0])
                #     return [EventTypeWindow, time, WindowEventMetricChange,
                #             0, 0, int(self.screen.c_w), int(self.screen.c_h), 0]
                # elif c_type == RSDL.VIDEOEXPOSE:
                #     self._deferred_events([EventTypeWindow, time, WindowEventPaint,
                #                             0, 0, int(self.screen.c_w), int(self.screen.c_h), 0])
                #     return [EventTypeWindow, time, WindowEventActivated, 0, 0, 0, 0, 0]
                elif event_type == RSDL.QUIT:
                    return [EventTypeWindow, time, WindowEventClose, 0, 0, 0, 0, 0]
        finally:
            lltype.free(event, flavor='raw')
        return [EventTypeNone, 0, 0, 0, 0, 0, 0, 0]

    def is_control_key(self, key_ord):
        return key_ord < 32 or key_ord in [
                key_constants.DELETE,
                key_constants.NUMLOCK,
                key_constants.SCROLLLOCK
                ]

    def is_modifier_key(self, key_ord):
        return key_ord in [
                key_constants.COMMAND,
                key_constants.CTRL,
                key_constants.SHIFT
                ]

    def fix_key_code_case(self):
        if self.key <= 255:
            # key could be lowercase so far but at least Cog
            # generates uppercase key codes for KeyDown/KeyUp
            self.key = ord(chr(self.key).upper())

    # Old style event handling
    def pump_events(self):
        event = lltype.malloc(RSDL.Event, flavor="raw")
        try:
            if rffi.cast(lltype.Signed, RSDL.PollEvent(event)) == 1:
                c_type = event.c_type
                if c_type == RSDL.MOUSEBUTTONDOWN or c_type == RSDL.MOUSEBUTTONUP:
                    self.handle_mouse_button(c_type, event)
                    return
                elif c_type == RSDL.MOUSEMOTION:
                    self.handle_mouse_move(c_type, event)
                elif c_type == RSDL.KEYDOWN:
                    self.handle_keyboard_event(c_type, event)
                    return
                elif c_type == RSDL.QUIT:
                    from spyvm.error import Exit
                    raise Exit("Window closed")
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
            modifier |= CommandKeyBit
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
    """Cursor modification not yet implemented in RSDL2?"""
    _attrs_ = ["cursor", "has_cursor", "has_display"]

    instance = None

    def __init__(self):
        # self.cursor = lltype.nullptr(RSDL.CursorPtr.TO)
        self.has_cursor = False
        self.has_display = False

    def set(self, data_words, w, h, x, y, mask_words=None):
        if not self.has_display:
            return
        if self.has_cursor:
            pass
            # RSDL.FreeCursor(self.cursor)
        data = self.words_to_bytes(len(data_words) * 4, data_words)
        try:
            mask = self.words_to_bytes(len(data_words) * 4, mask_words)
            try:
                # self.cursor = RSDL.CreateCursor(data, mask, w * 2, h, x, y)
                self.has_cursor = True
                # RSDL.SetCursor(self.cursor)
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
