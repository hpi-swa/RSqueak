import sys

from rpython.rlib.unroll import unrolling_iterable
from rpython.rlib.rarithmetic import r_uint, intmask
from rpython.rtyper.lltypesystem import lltype, rffi
# from rpython.rlib.runicode import unicode_encode_utf_8

from rsdl import RSDL
from rsqueakvm.util import system
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
EventTypeMouseWheel = 7

EventDragTypeEnter = 1
EventDragTypeMove = 2
EventDragTypeLeave = 3
EventDragTypeDrop = 4

EventKeyChar = 0
EventKeyDown = 1
EventKeyUp = 2

WindowEventMetricChange = 1
WindowEventClose = 2
WindowEventIconise = 3
WindowEventActivated = 4
WindowEventPaint = 5
WindowEventStinks = 6

DISABLED_EVENTS = unrolling_iterable([
    "FINGERDOWN", "FINGERUP", "FINGERMOTION", "TEXTEDITING"
])

MINIMUM_DEPTH = 16
BELOW_MINIMUM_DEPTH = 32

PIXELVOIDPP = lltype.malloc(rffi.VOIDPP.TO, 1,
                            flavor='raw', zero=True, immortal=True)
PITCHINTP = lltype.malloc(rffi.INTP.TO, 1,
                          flavor='raw', zero=True, immortal=True)
FLIP_RECT = lltype.malloc(RSDL.Rect, flavor='raw', zero=True, immortal=True)
RENDER_RECT = lltype.malloc(RSDL.Rect, flavor='raw', zero=True, immortal=True)

DEPTH_TO_PIXELFORMAT = {
    16: RSDL.PIXELFORMAT_ARGB1555,
    32: RSDL.PIXELFORMAT_ARGB8888
}


class SqueakInterrupt(Exception):
    pass


class NullDisplay(object):
    _attrs_ = ['button', 'mouse_position', 'key', 'width', 'height', 'depth',
               'pitch']

    def __init__(self):
        self.button = 0
        self.mouse_position = [0, 0]
        self.key = 0
        self.width = 0
        self.height = 0
        self.depth = 32
        self.pitch = 0

    def defer_updates(self, flag):
        pass

    def get_pixelbuffer(self):
        raise RuntimeError('Code path should not be reached')

    def get_plain_pixelbuffer(self):
        raise RuntimeError('Code path should not be reached')

    def get_next_event(self, time=0):
        return [EventTypeNone, 0, 0, 0, 0, 0, 0, 0]

    def flip(self, pixels, x, y, x2, y2):
        pass

    def render(self, force=False):
        pass

    def has_clipboard_text(self):
        return False

    def get_dropped_filename(self):
        return None

    def has_interrupts_pending(self):
        return False

    def is_headless(self):
        return True

    def mouse_button(self):
        return self.button

    def mouse_point(self):
        return self.mouse_position

    def next_keycode(self):
        return self.key

    def peek_keycode(self):
        return self.key

    def set_full_screen(self, flag):
        pass

    def set_title(self, title):
        pass

    def set_interrupt_key(self, space, encoded_key):
        pass

    def set_clipboard_text(self, text):
        return True

    def set_video_mode(self, w, h, d):
        pass


class SDLDisplay(NullDisplay):
    _attrs_ = ["window", "title", "renderer", "screen_texture", "altf4quit",
               "interrupt_key", "_dropped_file", "_defer_updates",
               "_deferred_events", "bpp", "pitch", "highdpi",
               "software_renderer", "interrupt_flag", "_texture_dirty"]
    _immutable_fields_ = ["interrupt_flag"]

    def __init__(self, title, highdpi, software_renderer, altf4quit):
        NullDisplay.__init__(self)
        self._init_sdl()
        self.title = title
        self.highdpi = highdpi
        self.software_renderer = software_renderer
        self.altf4quit = altf4quit
        SDLCursor.has_display = True
        self.window = lltype.nullptr(RSDL.WindowPtr.TO)
        self.renderer = lltype.nullptr(RSDL.RendererPtr.TO)
        self.screen_texture = lltype.nullptr(RSDL.TexturePtr.TO)
        # pushing all four meta keys, of which we support three...
        self.interrupt_key = 15 << 8
        self._deferred_events = []
        self.insert_padding_event()
        self._defer_updates = False
        self._texture_dirty = True
        self._dropped_file = None

    def _init_sdl(self):
        if RSDL.Init(RSDL.INIT_VIDEO) < 0:
            print RSDL.GetError()
            assert False
        self.interrupt_flag = lltype.malloc(rffi.SIGNEDP.TO, 1, flavor='raw')
        self.interrupt_flag[0] = 0
        ll_SetEventFilter(self.interrupt_flag)
        # do not wait for vsync
        RSDL.SetHint(RSDL.HINT_RENDER_VSYNC, '0')
        # nearest pixel sampling
        RSDL.SetHint(RSDL.HINT_RENDER_SCALE_QUALITY, '0')
        # disable WM_PING, so the WM does not think we're hung
        RSDL.SetHint(RSDL.HINT_VIDEO_X11_NET_WM_PING, '0')
        # Ctrl-Click on Mac is right click
        RSDL.SetHint(RSDL.HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK, '1')
        for eventname in DISABLED_EVENTS:
            RSDL.EventState(getattr(RSDL, eventname), RSDL.IGNORE)
        # try to allow late tearing (pushes frames faster)
        if (RSDL.SetSwapInterval(-1) < 0):
            RSDL.SetSwapInterval(0)  # at least try to disable vsync

    def has_interrupts_pending(self):
        if self.interrupt_flag[0] != 0:
            self.interrupt_flag[0] = 0
            return True
        return False

    def is_headless(self):
        return False

    def close(self):
        RSDL.Quit()

    def create_window_and_renderer(self, x, y, width, height):
        flags = RSDL.WINDOW_RESIZABLE
        if self.highdpi:
            flags |= RSDL.WINDOW_ALLOW_HIGHDPI
        self.window = RSDL.CreateWindow(self.title, x, y, width, height, flags)
        # Use software renderer for now to avoid problems w/ duplicate buffers
        self.renderer = RSDL.CreateRenderer(self.window, -1,
                                            RSDL.RENDERER_SOFTWARE)

    def set_video_mode(self, w, h, d):
        if not (w > 0 and h > 0):
            return
        assert d in (1, 2, 4, 8, 16, 32)
        if d < MINIMUM_DEPTH:
            d = BELOW_MINIMUM_DEPTH
        self.width = intmask(w)
        self.height = intmask(h)
        self.depth = intmask(d)
        if self.window == lltype.nullptr(RSDL.WindowPtr.TO):
            self.create_window_and_renderer(x=RSDL.WINDOWPOS_UNDEFINED,
                                            y=RSDL.WINDOWPOS_UNDEFINED,
                                            width=w,
                                            height=h)
        if self.screen_texture != lltype.nullptr(RSDL.TexturePtr.TO):
            RSDL.DestroyTexture(self.screen_texture)
        self.screen_texture = RSDL.CreateTexture(
            self.renderer,
            DEPTH_TO_PIXELFORMAT[d], RSDL.TEXTUREACCESS_STREAMING,
            w, h)
        if not self.screen_texture:
            print 'Could not create screen texture'
            raise RuntimeError(RSDL.GetError())
        self.lock()
        if d == 16:
            self.bpp = 2
        elif d == 32:
            self.bpp = 4
        else:
            assert False
        self.pitch = self.width * self.bpp
        self.full_damage()

    def set_full_screen(self, flag):
        if flag:
            RSDL.SetWindowFullscreen(self.window,
                                     RSDL.WINDOW_FULLSCREEN_DESKTOP)
        else:
            RSDL.SetWindowFullscreen(self.window, 0)

    def set_title(self, title):
        RSDL.SetWindowTitle(self.window, title)

    def defer_updates(self, flag):
        self._defer_updates = flag

    def flip(self, pixels, x, y, x2, y2):
        self.copy_pixels(pixels, x + y * self.width, x2 + y2 * self.width)
        self.record_damage(x, y, x2 - x, y2 - y)
        self._texture_dirty = True

    def render(self, force=False):
        if not force:
            if self._defer_updates or not self._texture_dirty:
                return
        self._texture_dirty = False
        self.unlock()
        ec = RSDL.RenderCopy(
            self.renderer,
            self.screen_texture,
            RENDER_RECT,
            RENDER_RECT)
        if ec != 0:
            print RSDL.GetError()
            return
        RSDL.RenderPresent(self.renderer)
        self.reset_damage()
        self.lock()

    def copy_pixels(self, pixels, start, stop):
        offset = start * self.bpp
        assert offset >= 0
        remaining_size = (self.width * self.height * self.bpp) - offset
        if remaining_size <= 0 or start >= stop:
            return
        nbytes = rffi.r_size_t(min((stop - start) * self.bpp, remaining_size))
        pixbuf = rffi.ptradd(PIXELVOIDPP[0], offset)
        surfacebuf = rffi.ptradd(rffi.cast(rffi.VOIDP, pixels), offset)
        rffi.c_memcpy(pixbuf, surfacebuf, nbytes)

    def record_damage(self, x, y, w, h):
        FLIP_RECT.c_x = rffi.r_int(x)
        FLIP_RECT.c_y = rffi.r_int(y)
        FLIP_RECT.c_w = rffi.r_int(min(w + 1, self.width - x))
        FLIP_RECT.c_h = rffi.r_int(min(h + 1, self.height - y))
        RSDL.UnionRect(FLIP_RECT, RENDER_RECT, RENDER_RECT)

    def reset_damage(self):
        RENDER_RECT.c_x = rffi.r_int(0)
        RENDER_RECT.c_y = rffi.r_int(0)
        RENDER_RECT.c_w = rffi.r_int(0)
        RENDER_RECT.c_h = rffi.r_int(0)

    def full_damage(self):
        RENDER_RECT.c_x = rffi.r_int(0)
        RENDER_RECT.c_y = rffi.r_int(0)
        RENDER_RECT.c_w = rffi.r_int(self.width)
        RENDER_RECT.c_h = rffi.r_int(self.height)

    def lock(self):
        ec = RSDL.LockTexture(
            self.screen_texture,
            lltype.nullptr(RSDL.Rect),
            PIXELVOIDPP,
            PITCHINTP)
        if ec != 0:
            print RSDL.GetError()
            return

    def unlock(self):
        RSDL.UnlockTexture(self.screen_texture)

    def handle_mouse_button(self, c_type, event):
        b = rffi.cast(RSDL.MouseButtonEventPtr, event)
        btn = r_uint(b.c_button)
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
        x = intmask(m.c_x)
        y = intmask(m.c_y)
        self.mouse_position = [x, y]

    def handle_keyboard_event(self, c_type, event):
        key = 0
        p = rffi.cast(RSDL.KeyboardEventPtr, event)
        sym = r_uint(p.c_keysym.c_sym)
        if sym == RSDL.K_DOWN:
            key = key_constants.DOWN
        elif sym == RSDL.K_LEFT:
            key = key_constants.LEFT
        elif sym == RSDL.K_RIGHT:
            key = key_constants.RIGHT
        elif sym == RSDL.K_UP:
            key = key_constants.UP
        elif sym == RSDL.K_HOME:
            key = key_constants.HOME
        elif sym == RSDL.K_END:
            key = key_constants.END
        elif sym == RSDL.K_INSERT:
            key = key_constants.INSERT
        elif sym == RSDL.K_PAGEUP:
            key = key_constants.PAGEUP
        elif sym == RSDL.K_PAGEDOWN:
            key = key_constants.PAGEDOWN
        elif sym == RSDL.K_LSHIFT or sym == RSDL.K_RSHIFT:
            key = key_constants.SHIFT
        elif sym == RSDL.K_LCTRL or sym == RSDL.K_RCTRL:
            key = key_constants.CTRL
        elif sym == RSDL.K_LALT or sym == RSDL.K_RALT:
            key = key_constants.COMMAND
        elif system.IS_DARWIN and (sym == RSDL.K_LGUI or sym == RSDL.K_RGUI):
            key = key_constants.COMMAND
        elif sym == RSDL.K_DELETE:
            key = key_constants.DELETE
        elif sym == RSDL.K_BACKSPACE:
            key = key_constants.BACKSPACE
        elif sym == RSDL.K_PAUSE:
            key = key_constants.BREAK
        elif sym == RSDL.K_CAPSLOCK:
            key = key_constants.CAPSLOCK
        elif sym == RSDL.K_NUMLOCKCLEAR:
            key = key_constants.NUMLOCK
        elif sym == RSDL.K_SCROLLLOCK:
            key = key_constants.SCROLLLOCK
        elif sym == RSDL.K_PRINTSCREEN:
            key = key_constants.PRINT
        else:
            key = rffi.cast(rffi.INT, sym)  # use SDL's keycode
            # this is the lowercase ascii-value for the most common keys
        # elif char != 0:
        #     chars = unicode_encode_utf_8(unichr(char), 1, 'ignore')
        #     if len(chars) == 1:
        #         asciivalue = ord(chars[0])
        #         if asciivalue >= 32:
        #             key = asciivalue
        if intmask(key) == 0 and sym <= r_uint(255):
            self.key = intmask(sym)
        else:
            self.key = intmask(key)
        interrupt = self.interrupt_key
        if (interrupt & 0xFF == self.key and
                interrupt >> 8 == self.get_modifier_mask(0)):
            raise SqueakInterrupt

    def handle_textinput_event(self, event):
        textinput = rffi.cast(RSDL.TextInputEventPtr, event)
        self.key = ord(
            rffi.charp2str(rffi.cast(rffi.CCHARP, textinput.c_text))[0])
        # XXX: textinput.c_text could contain multiple characters
        #      so probably multiple Squeak events must be emitted
        #      moreover, this is UTF-8 so umlauts etc. have to be decoded

    def handle_windowevent(self, c_type, event):
        window_event = rffi.cast(RSDL.WindowEventPtr, event)
        if r_uint(window_event.c_event) in (RSDL.WINDOWEVENT_RESIZED,
                                            RSDL.WINDOWEVENT_SIZE_CHANGED,
                                            RSDL.WINDOWEVENT_EXPOSED):
            neww = intmask(window_event.c_data1)
            newh = intmask(window_event.c_data2)
            if neww != self.width or newh != self.height:
                self.set_video_mode(w=neww, h=newh, d=self.depth)
            self.full_damage()
            self.render(force=True)

    def get_dropevent(self, time, c_type, event):
        drop_event = rffi.cast(RSDL.DropEventPtr, event)
        path = rffi.charp2str(drop_event.c_file)
        btn, mods = self.get_mouse_event_buttons_and_mods()
        self._dropped_file = path
        lltype.free(drop_event.c_file, flavor='raw')
        return [EventTypeDragDropFiles,
                time,
                EventDragTypeDrop,
                int(self.mouse_position[0]),
                int(self.mouse_position[1]),
                mods,
                1]

    def get_dropped_filename(self):
        return self._dropped_file

    def get_mouse_event_buttons_and_mods(self):
        mods = self.get_modifier_mask(3)
        btn = self.button
        if btn == RedButtonBit:
            if mods & CtrlKeyBit:
                btn = BlueButtonBit
            elif mods & (CommandKeyBit | OptionKeyBit):
                btn = YellowButtonBit
        return (btn, mods)

    def get_next_mouse_event(self, time):
        btn, mods = self.get_mouse_event_buttons_and_mods()
        return [EventTypeMouse,
                time,
                int(self.mouse_position[0]),
                int(self.mouse_position[1]),
                btn,
                mods,
                0,
                0]

    def get_next_mouse_wheel_event(self, time, event):
        btn, mods = self.get_mouse_event_buttons_and_mods()
        e = rffi.cast(RSDL.MouseWheelEventPtr, event)
        return [EventTypeMouseWheel,
                time,
                intmask(e.c_x) * 120,
                intmask(e.c_y) * 120,
                btn,
                mods,
                0,
                0]

    def get_next_key_event(self, key_event_type, time):
        mods = self.get_modifier_mask(0)
        return [EventTypeKeyboard,
                time,
                self.key,
                key_event_type,
                mods,
                self.key,
                0,
                0]

    def get_next_event(self, time=0):
        if self.has_queued_events():
            return self.dequeue_event()
        got_event = False
        with lltype.scoped_alloc(RSDL.Event) as event:
            while RSDL.PollEvent(event) == 1:
                got_event = True
                event_type = r_uint(event.c_type)
                if event_type in (RSDL.MOUSEBUTTONDOWN, RSDL.MOUSEBUTTONUP):
                    self.handle_mouse_button(event_type, event)
                    self.queue_event(self.get_next_mouse_event(time))
                elif event_type == RSDL.MOUSEMOTION:
                    self.handle_mouse_move(event_type, event)
                    self.queue_event(self.get_next_mouse_event(time))
                elif event_type == RSDL.MOUSEWHEEL:
                    self.queue_event(self.get_next_mouse_wheel_event(time, event))
                elif event_type == RSDL.KEYDOWN:
                    self.handle_keyboard_event(event_type, event)
                    later = None
                    if not self.is_modifier_key(self.key):
                        # no TEXTINPUT event for this key will follow, but
                        # Squeak needs a KeyStroke anyway
                        if ((system.IS_LINUX and
                            self.is_control_key(self.key)) or
                            (not system.IS_LINUX and
                             (self.is_control_key(self.key) or
                              RSDL.GetModState() & ~RSDL.KMOD_SHIFT != 0))):
                            later = self.get_next_key_event(EventKeyChar, time)
                    self.fix_key_code_case()
                    self.queue_event(self.get_next_key_event(EventKeyDown, time))
                    if later:
                        self.insert_padding_event()
                        self.queue_event(later)
                elif event_type == RSDL.TEXTINPUT:
                    self.handle_textinput_event(event)
                    self.queue_event(self.get_next_key_event(EventKeyChar, time))
                elif event_type == RSDL.KEYUP:
                    self.handle_keyboard_event(event_type, event)
                    self.fix_key_code_case()
                    self.queue_event(self.get_next_key_event(EventKeyUp, time))
                elif event_type == RSDL.WINDOWEVENT:
                    self.handle_windowevent(event_type, event)
                elif event_type == RSDL.DROPFILE:
                    self.queue_event(self.get_dropevent(time, event_type, event))
                elif event_type == RSDL.QUIT:
                    if self.altf4quit:  # we want to quit hard
                        from rsqueakvm.util.dialog import ask_question
                        if ask_question('Quit Squeak without saving?'):
                            raise Exception
                    self.queue_event([
                        EventTypeWindow, time, WindowEventClose, 0, 0, 0, 0, 0
                    ])
                elif event_type in (RSDL.RENDER_TARGETS_RESET, RSDL.RENDER_DEVICE_RESET):
                    self.full_damage()
                    self.render(force=True)
                self.insert_padding_event()
        if got_event:
            return self.dequeue_event()
        return [EventTypeNone, 0, 0, 0, 0, 0, 0, 0]

    def has_queued_events(self):
        return len(self._deferred_events) > 0

    def queue_event(self, evt):
        self._deferred_events.append(evt)

    def dequeue_event(self):
        return self._deferred_events.pop(0)

    def insert_padding_event(self):
        # we always return one None event between every event, so we poll only
        # half of the time
        self.queue_event([EventTypeNone, 0, 0, 0, 0, 0, 0, 0])

    def is_control_key(self, key_ord):
        key_ord = key_ord
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
        event = lltype.malloc(RSDL.Event, flavor='raw')
        try:
            if RSDL.PollEvent(event) == 1:
                c_type = r_uint(event.c_type)
                if (c_type == r_uint(RSDL.MOUSEBUTTONDOWN) or
                        c_type == r_uint(RSDL.MOUSEBUTTONUP)):
                    self.handle_mouse_button(c_type, event)
                    return
                elif c_type == r_uint(RSDL.MOUSEMOTION):
                    self.handle_mouse_move(c_type, event)
                elif c_type == r_uint(RSDL.KEYDOWN):
                    self.handle_keyboard_event(c_type, event)
                    return
                elif c_type == r_uint(RSDL.QUIT):
                    from rsqueakvm.error import Exit
                    raise Exit('Window closed')
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
        if mod & RSDL.KMOD_CAPS != 0:
            modifier |= ShiftKeyBit
        if mod & RSDL.KMOD_ALT != 0:
            if not system.IS_DARWIN:
                modifier |= CommandKeyBit
            else:
                modifier |= OptionKeyBit
        if mod & RSDL.KMOD_GUI != 0:
            modifier |= CommandKeyBit
        return intmask(modifier << shift)

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

    def get_clipboard_text(self):
        return rffi.charp2str(RSDL.GetClipboardText())

    def set_clipboard_text(self, text):
        return RSDL.SetClipboardText(rffi.str2charp(text))

    def has_clipboard_text(self):
        return RSDL.HasClipboardText() == RSDL.TRUE


class SDLCursorClass(object):
    """Cursor modification not yet implemented in RSDL2?"""
    _attrs_ = ['cursor', 'has_cursor', 'has_display']

    instance = None

    def __init__(self):
        self.cursor = lltype.nullptr(RSDL.CursorPtr.TO)
        self.has_cursor = False
        self.has_display = False

    def set(self, data_words, w, h, x, y, mask_words=None):
        if not self.has_display:
            return True
        if self.has_cursor:
            RSDL.FreeCursor(self.cursor)
        bytenum = len(data_words) * 2
        data = self.cursor_words_to_bytes(bytenum, data_words)
        try:
            mask = self.cursor_words_to_bytes(bytenum, mask_words)
            try:
                self.cursor = RSDL.CreateCursor(data, mask, w, h, x, y)
                if self.cursor == lltype.nullptr(RSDL.CursorPtr.TO):
                    print RSDL.GetError()
                    return False
                self.has_cursor = True
                RSDL.SetCursor(self.cursor)
            finally:
                lltype.free(mask, flavor='raw')
        finally:
            lltype.free(data, flavor='raw')
        return True

    def cursor_words_to_bytes(self, bytenum, words):
        """In Squeak, only the upper 16bits of the cursor form seem to count
        (I'm guessing because the code was ported over from 16-bit machines),
        so this ignores the lower 16-bits of each word."""
        bytes = lltype.malloc(RSDL.Uint8P.TO, bytenum, flavor='raw')
        if words:
            for idx, word in enumerate(words):
                bytes[idx * 2] = rffi.r_uchar((word >> 24) & 0xff)
                bytes[idx * 2 + 1] = rffi.r_uchar((word >> 16) & 0xff)
        else:
            for idx in range(bytenum):
                bytes[idx] = rffi.r_uchar(0)
        return bytes

if 'sphinx' not in sys.modules:
    SDLCursor = SDLCursorClass()

    from rpython.translator.tool.cbuild import ExternalCompilationInfo
    from rsdl.eci import get_rsdl_compilation_info
    eci = ExternalCompilationInfo(
        post_include_bits=["""
        #ifndef __event_filter_h
        #define __event_filter_h

        #ifdef _WIN32
        #define DLLEXPORT __declspec(dllexport)
        #else
        #define DLLEXPORT __attribute__((__visibility__("default")))
        #endif

        #ifdef __cplusplus
        extern "C" {
        #endif
                DLLEXPORT int SetEventFilter(intptr_t* target);
        #ifdef __cplusplus
        }
        #endif

        #endif"""],
        separate_module_sources=["""
        int InterruptEventFilter(void* userdata, SDL_Event *event) {
            int interrupt_key = 15 << 8;
            if (event->type == SDL_KEYUP) {
                if (((SDL_KeyboardEvent*)event)->keysym.sym == SDLK_PERIOD) {
                    if ((((SDL_KeyboardEvent*)event)->keysym.mod & (KMOD_ALT|KMOD_GUI)) != 0) {
                        if (event->type == SDL_KEYUP) { // only keyup generates the interrupt
                            ((intptr_t*)userdata)[0] = 1;
                            // an interrupt flushes all pending events preceding it, so we don't
                            // get spurious events processing when the debugger opens
                            SDL_FlushEvents(SDL_FIRSTEVENT, SDL_LASTEVENT);
                        }
                        return 0;
                    }
                }
            }
            return 1;
        }
        int SetEventFilter(intptr_t* target) {
            SDL_SetEventFilter(InterruptEventFilter, (void*)target);
            return 0;
        }
        """]
    ).merge(get_rsdl_compilation_info())

    ll_SetEventFilter = rffi.llexternal('SetEventFilter', [rffi.SIGNEDP],
                                        rffi.INT, compilation_info=eci)
