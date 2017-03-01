import py, os, math, time
import pytest
from rpython.rlib.rarithmetic import intmask, r_uint
from rpython.rtyper.lltypesystem import lltype, rffi
from rsdl import RSDL
from rsqueakvm import display, key_constants
from rsqueakvm.util import system

@pytest.fixture(autouse=True)
def stub_sdl(monkeypatch):
    monkeypatch.setattr(RSDL, "PollEvent", lambda *args: 0)
    monkeypatch.setattr(RSDL, "Init", lambda *args: 0)
    monkeypatch.setattr(RSDL, "Quit", lambda: 0)

@pytest.fixture
def mocked_sdl_event_queue(monkeypatch):
    from collections import deque
    queue = deque()
    def PollEvent_stub(target_event):
        if len(queue) == 0:
            return 0
        next_event = queue.popleft()
        rffi.c_memcpy(target_event, next_event, rffi.sizeof(RSDL.Event))
        return 1
    monkeypatch.setattr(RSDL, "PollEvent", PollEvent_stub)
    return queue

class EventFactory(object):
    def __init__(self):
        self.events = []

    def malloc(self, type=RSDL.Event):
        event = lltype.malloc(type, flavor='raw')
        self.events.append(event)
        return event

    def free_all(self):
        for event in self.events:
            lltype.free(event, flavor='raw')

@pytest.yield_fixture
def stub_events():
    factory = EventFactory()
    try:
        yield factory
    finally:
        factory.free_all()

@pytest.fixture
def stub_key_event(stub_events):
    p_testkeyevent = rffi.cast(RSDL.KeyboardEventPtr, stub_events.malloc())
    return p_testkeyevent

@pytest.fixture
def stub_textinput_event(stub_events):
    testinputevent = rffi.cast(RSDL.TextInputEventPtr, stub_events.malloc())
    return testinputevent

@pytest.fixture
def stub_window_event(stub_events):
    p_test_window_event = rffi.cast(RSDL.WindowEventPtr, stub_events.malloc())
    p_test_window_event.c_type = RSDL.WINDOWEVENT
    return p_test_window_event

class ModHolder(object):
    def __init__(self):
        self.modstate = 0
    def set(self, value):
        self.modstate = value

@pytest.fixture
def stub_mod_state(monkeypatch):
    mods = ModHolder()
    monkeypatch.setattr(RSDL, 'GetModState', lambda: mods.modstate)
    return mods

@pytest.fixture
def sut():
    d = display.SDLDisplay("test", True, False, False)
    def get_next_event(time=0):
        # skip the none-event we always insert
        display.SDLDisplay.get_next_event(d, time)
        return display.SDLDisplay.get_next_event(d, time)
    d.get_next_event = get_next_event
    return d

def assert_keyevent_array(actual, expected_char_code=None,
        expected_key_event_type=None, expected_modifiers=None):
    assert len(actual) == 8
    assert actual[0] != 0, "event polled unexpectedly"
    assert actual[0] == display.EventTypeKeyboard
    if expected_char_code:
        assert actual[2] == expected_char_code
    if expected_key_event_type:
        assert actual[3] == expected_key_event_type
    if expected_modifiers:
        assert actual[4] == expected_modifiers

def test_keypresses(sut, mocked_sdl_event_queue, stub_events):
    assert_keypress(sut, mocked_sdl_event_queue, stub_events, RSDL.K_a, 'a')
    assert_keypress(sut, mocked_sdl_event_queue, stub_events, RSDL.K_b, 'b')

## jr: Would it be more appropriate to raise a KeyDown event only once when the
##     user actually pushes the key down? Currently, the event is reraised
##     whenever the KeyChar event is reraised because of repeated keys.

def assert_keypress(sut, mocked_sdl_event_queue, stub_events, sdl_key, char):
    # given
    keydown = stub_events.malloc(RSDL.KeyboardEvent)
    keydown.c_type = RSDL.KEYDOWN
    keydown.c_keysym.c_sym = rffi.r_int(sdl_key)
    textinput = stub_events.malloc(RSDL.TextInputEvent)
    textinput.c_type = RSDL.TEXTINPUT
    rffi.str2chararray(str(char) + '\x00', textinput.c_text,
            RSDL.TEXTINPUTEVENT_TEXT_SIZE)
    keyup = stub_events.malloc(RSDL.KeyboardEvent)
    keyup.c_type = RSDL.KEYUP
    keyup.c_keysym.c_sym = rffi.r_int(sdl_key)
    mocked_sdl_event_queue.append(keydown)
    mocked_sdl_event_queue.append(textinput)
    mocked_sdl_event_queue.append(keyup)
    # when
    first_event = sut.get_next_event()
    second_event = sut.get_next_event()
    third_event = sut.get_next_event()
    # then
    assert_keyevent_array(first_event, ord(char.upper()), display.EventKeyDown, 0)
    assert_keyevent_array(second_event, ord(char), display.EventKeyChar, 0)
    assert_keyevent_array(third_event, ord(char.upper()), display.EventKeyUp, 0)

def assert_keydownup(display_under_test, mocked_sdl_event_queue, stub_events, sdl_key, char):
    assert char.isupper() or not char.isalpha(), \
            "asserted char for KeyDown and KeyUp must be uppercase"
    # given
    keydown = stub_events.malloc(RSDL.KeyboardEvent)
    keydown.c_type = RSDL.KEYDOWN
    keydown.c_keysym.c_sym = rffi.r_int(sdl_key)
    keyup = stub_events.malloc(RSDL.KeyboardEvent)
    keyup.c_type = RSDL.KEYUP
    keyup.c_keysym.c_sym = rffi.r_int(sdl_key)
    mocked_sdl_event_queue.append(keydown)
    mocked_sdl_event_queue.append(keyup)
    # when
    sqKeyDown = display_under_test.get_next_event()
    sqKeyStroke = display_under_test.get_next_event()
    sqKeyUp = display_under_test.get_next_event()
    # then
    assert_keyevent_array(sqKeyDown, ord(char), display.EventKeyDown, 0)
    assert_keyevent_array(sqKeyStroke, ord(char), display.EventKeyChar, 0)
    assert_keyevent_array(sqKeyUp, ord(char), display.EventKeyUp, 0)

def test_modifiers_do_not_cause_keychar_event(sut, mocked_sdl_event_queue, stub_key_event, stub_mod_state):
    def assert_key(key, sdl_mod, char_code, modifiers):
        assert_does_not_generate_keychar_events(sut, mocked_sdl_event_queue, stub_key_event, stub_mod_state, key, sdl_mod, char_code, modifiers)
    assert_key(RSDL.K_LSHIFT, RSDL.KMOD_LSHIFT, key_constants.SHIFT, display.ShiftKeyBit)
    assert_key(RSDL.K_RSHIFT, RSDL.KMOD_RSHIFT, key_constants.SHIFT, display.ShiftKeyBit)
    assert_key(RSDL.K_LCTRL, RSDL.KMOD_LCTRL, key_constants.CTRL, display.CtrlKeyBit)
    assert_key(RSDL.K_RCTRL, RSDL.KMOD_RCTRL, key_constants.CTRL, display.CtrlKeyBit)
    if system.IS_DARWIN:
        assert_key(RSDL.K_LGUI, RSDL.KMOD_LGUI, key_constants.COMMAND, display.CommandKeyBit)
        assert_key(RSDL.K_RGUI, RSDL.KMOD_RGUI, key_constants.COMMAND, display.CommandKeyBit)
    else:
        assert_key(RSDL.K_LALT, RSDL.KMOD_LALT, key_constants.COMMAND, display.CommandKeyBit)
        assert_key(RSDL.K_RALT, RSDL.KMOD_RALT, key_constants.COMMAND, display.CommandKeyBit)
    assert_key(RSDL.K_LGUI, RSDL.KMOD_NONE, None, None)
    assert_key(RSDL.K_RGUI, RSDL.KMOD_NONE, None, None)

def assert_does_not_generate_keychar_events(sut, mocked_sdl_event_queue, stub_key_event, stub_mod_state, key, sdl_modifier, expected_char_code, expected_modifiers):
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYDOWN)
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', key)
    stub_mod_state.set(sdl_modifier)
    mocked_sdl_event_queue.append(stub_key_event)
    result = sut.get_next_event()
    assert_keyevent_array(result, expected_char_code, display.EventKeyDown, expected_modifiers)
    mocked_sdl_event_queue.append(stub_key_event)
    result = sut.get_next_event()
    assert_keyevent_array(result, expected_char_code, display.EventKeyDown, expected_modifiers)
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYUP)
    stub_mod_state.set(RSDL.KMOD_NONE)
    mocked_sdl_event_queue.append(stub_key_event)
    result = sut.get_next_event()
    assert_keyevent_array(result, expected_char_code, display.EventKeyUp, 0)

def test_movement_keys(sut, mocked_sdl_event_queue, stub_events):
    def assert_key(key, expected_char_code):
        assert_keydownup(sut, mocked_sdl_event_queue, stub_events, key, chr(expected_char_code))
    assert_key(RSDL.K_LEFT, key_constants.LEFT)
    assert_key(RSDL.K_RIGHT, key_constants.RIGHT)
    assert_key(RSDL.K_UP, key_constants.UP)
    assert_key(RSDL.K_DOWN, key_constants.DOWN)
    assert_key(RSDL.K_HOME, key_constants.HOME)
    assert_key(RSDL.K_END, key_constants.END)
    assert_key(RSDL.K_PAGEUP, key_constants.PAGEUP)
    assert_key(RSDL.K_PAGEDOWN, key_constants.PAGEDOWN)

def test_other_keys(sut, mocked_sdl_event_queue, stub_events):
    def assert_key(key, expected_char_code):
        assert_keydownup(sut, mocked_sdl_event_queue, stub_events, key, chr(expected_char_code))
    assert_key(RSDL.K_INSERT, key_constants.INSERT)
    assert_key(RSDL.K_RETURN, key_constants.RETURN)
    assert_key(RSDL.K_PAUSE, key_constants.BREAK)
    assert_key(RSDL.K_CAPSLOCK, key_constants.CAPSLOCK)
    assert_key(RSDL.K_ESCAPE, key_constants.ESCAPE)
    #assert_key(RSDL.K_PRINTSCREEN, key_constants.PRINT)  # how to try it out?
    assert_key(RSDL.K_BACKSPACE, key_constants.BACKSPACE)
    assert_key(RSDL.K_DELETE, key_constants.DELETE)
    assert_key(RSDL.K_NUMLOCKCLEAR, key_constants.NUMLOCK)
    assert_key(RSDL.K_SCROLLLOCK, key_constants.SCROLLLOCK)

# see Character class methods (delete, end, home, ...)
# and Editor class >> initializeKeystrokeActions
# char code can be different between keyChar and keyDown events (as we know from SWA)

def test_entering_captital_letters(sut, mocked_sdl_event_queue, stub_events, stub_mod_state):
    # when
    # shift down
    shift_down = stub_events.malloc(RSDL.KeyboardEvent)
    shift_down.c_type = RSDL.KEYDOWN
    shift_down.c_keysym.c_sym = rffi.r_int(RSDL.K_LSHIFT)
    mocked_sdl_event_queue.append(shift_down)
    stub_mod_state.set(RSDL.KMOD_SHIFT)
    sqShiftDown = sut.get_next_event()
    # A down
    a_down = stub_events.malloc(RSDL.KeyboardEvent)
    a_down.c_type = RSDL.KEYDOWN
    a_down.c_keysym.c_sym = rffi.r_int(RSDL.K_a)
    mocked_sdl_event_queue.append(a_down)
    sqADown = sut.get_next_event()
    # A entered
    a_textinput = stub_events.malloc(RSDL.TextInputEvent)
    a_textinput.c_type = RSDL.TEXTINPUT
    rffi.str2chararray('A\x00', a_textinput.c_text, RSDL.TEXTINPUTEVENT_TEXT_SIZE)
    mocked_sdl_event_queue.append(a_textinput)
    sqAStroke = sut.get_next_event()
    # repeat A entered
    mocked_sdl_event_queue.append(a_down)
    mocked_sdl_event_queue.append(a_textinput)
    sqADown2 = sut.get_next_event()
    sqAStroke2 = sut.get_next_event()
    # A up
    a_up = stub_events.malloc(RSDL.KeyboardEvent)
    a_up.c_type = RSDL.KEYUP
    a_up.c_keysym.c_sym = rffi.r_int(RSDL.K_a)
    mocked_sdl_event_queue.append(a_up)
    sqAUp = sut.get_next_event()
    # shift up
    shift_up = stub_events.malloc(RSDL.KeyboardEvent)
    shift_up.c_type = RSDL.KEYUP
    shift_up.c_keysym.c_sym = rffi.r_int(RSDL.K_LSHIFT)
    mocked_sdl_event_queue.append(shift_up)
    stub_mod_state.set(RSDL.KMOD_NONE)
    sqShiftUp = sut.get_next_event()
    # then
    assert_keyevent_array(sqShiftDown, key_constants.SHIFT, display.EventKeyDown, display.ShiftKeyBit)
    assert_keyevent_array(sqADown, ord('A'), display.EventKeyDown, display.ShiftKeyBit)
    assert_keyevent_array(sqAStroke, ord('A'), display.EventKeyChar, display.ShiftKeyBit)
    assert_keyevent_array(sqADown2, ord('A'), display.EventKeyDown, display.ShiftKeyBit)
    assert_keyevent_array(sqAStroke2, ord('A'), display.EventKeyChar, display.ShiftKeyBit)
    assert_keyevent_array(sqAUp, ord('A'), display.EventKeyUp, display.ShiftKeyBit)
    assert_keyevent_array(sqShiftUp, key_constants.SHIFT, display.EventKeyUp, 0)

def test_keyboard_chords(sut, mocked_sdl_event_queue, stub_events, stub_mod_state):
    # when
    # CTRL down
    ctrl_down = stub_events.malloc(RSDL.KeyboardEvent)
    ctrl_down.c_type = RSDL.KEYDOWN
    ctrl_down.c_keysym.c_sym = rffi.r_int(RSDL.K_LCTRL)
    mocked_sdl_event_queue.append(ctrl_down)
    stub_mod_state.set(RSDL.KMOD_CTRL)
    sqCtrlDown = sut.get_next_event()
    # A down
    a_down = stub_events.malloc(RSDL.KeyboardEvent)
    a_down.c_type = RSDL.KEYDOWN
    a_down.c_keysym.c_sym = rffi.r_int(RSDL.K_a)
    mocked_sdl_event_queue.append(a_down)
    sqADown = sut.get_next_event()
    # A entered
    if system.IS_LINUX:
        # on linux, we don't get this
        a_stroke = stub_events.malloc(RSDL.TextInputEvent)
        a_stroke.c_type = RSDL.TEXTINPUT
        rffi.str2chararray('a\x00', a_stroke.c_text, RSDL.TEXTINPUTEVENT_TEXT_SIZE)
        mocked_sdl_event_queue.append(a_stroke)
    sqAStroke = sut.get_next_event()
    # repeat A
    mocked_sdl_event_queue.append(a_down)
    sqADown2 = sut.get_next_event()
    if system.IS_LINUX:
        # on linux, we don't get this
        a_stroke = stub_events.malloc(RSDL.TextInputEvent)
        a_stroke.c_type = RSDL.TEXTINPUT
        rffi.str2chararray('a\x00', a_stroke.c_text, RSDL.TEXTINPUTEVENT_TEXT_SIZE)
        mocked_sdl_event_queue.append(a_stroke)
    sqAStroke2 = sut.get_next_event()
    # A up
    a_up = stub_events.malloc(RSDL.KeyboardEvent)
    a_up.c_type = RSDL.KEYUP
    a_up.c_keysym.c_sym = rffi.r_int(RSDL.K_a)
    mocked_sdl_event_queue.append(a_up)
    sqAUp = sut.get_next_event()
    # CTRL up
    ctrl_up = stub_events.malloc(RSDL.KeyboardEvent)
    ctrl_up.c_type = RSDL.KEYUP
    ctrl_up.c_keysym.c_sym = rffi.r_int(RSDL.K_LCTRL)
    mocked_sdl_event_queue.append(ctrl_up)
    stub_mod_state.set(RSDL.KMOD_NONE)
    sqCtrlUp = sut.get_next_event()
    # then
    assert_keyevent_array(sqCtrlDown, key_constants.CTRL, display.EventKeyDown, display.CtrlKeyBit)
    assert_keyevent_array(sqADown, ord('A'), display.EventKeyDown, display.CtrlKeyBit)
    assert_keyevent_array(sqAStroke, ord('a'), display.EventKeyChar, display.CtrlKeyBit)
    assert_keyevent_array(sqADown2, ord('A'), display.EventKeyDown, display.CtrlKeyBit)
    assert_keyevent_array(sqAStroke2, ord('a'), display.EventKeyChar, display.CtrlKeyBit)
    assert_keyevent_array(sqAUp, ord('A'), display.EventKeyUp, display.CtrlKeyBit)
    assert_keyevent_array(sqCtrlUp, key_constants.CTRL, display.EventKeyUp, 0)

@pytest.fixture
def stub_screen_texture_creation(monkeypatch):
    capture_buffer = []
    last_returned_texture = [None]
    def capture_create_texture(renderer, format, access, w, h):
        capture_buffer.append({'function': RSDL.CreateTexture,
            'renderer': renderer,
            'format': format,
            'access': access,
            'width': w,
            'height': h})
        last_returned_texture[0] = lltype.Ptr(RSDL.TexturePtr.TO, None)
        return last_returned_texture[0]
    monkeypatch.setattr(RSDL, 'CreateTexture', capture_create_texture)
    def capture_destroy_texture(texture):
        # in addition to the texture pointer, store whether this is the
        # most recently created texture
        capture_buffer.append({'function': RSDL.DestroyTexture,
            'texture': texture,
            'is_last_created_texture': texture == last_returned_texture[0]})
    monkeypatch.setattr(RSDL, 'DestroyTexture', capture_destroy_texture)
    return capture_buffer

def xxxtest_window_resize_events(sut, mocked_sdl_event_queue, stub_window_event, stub_screen_texture_creation):
    def assert_updated_metrics(width, height):
        stub_window_event.c_event = RSDL.WINDOWEVENT_RESIZED
        stub_window_event.c_data1 = rffi.r_int(width)
        stub_window_event.c_data2 = rffi.r_int(height)
        mocked_sdl_event_queue.append(stub_window_event)
        result = sut.get_next_event()
        # TODO: decide whether no events or windowmetric events should be raised
        assert sut.width == width
        assert sut.height == height
    assert_updated_metrics(300, 200)
    assert_updated_metrics(1024, 768)

def test_display_resize_on_resize_events(sut, mocked_sdl_event_queue, stub_window_event,
        stub_screen_texture_creation, monkeypatch):
    def assert_updated_metrics(width, height, check_destroy,
            assert_destroyed_last=True):
        stub_window_event.c_event = RSDL.WINDOWEVENT_RESIZED
        stub_window_event.c_data1 = rffi.r_int(width)
        stub_window_event.c_data2 = rffi.r_int(height)
        mocked_sdl_event_queue.append(stub_window_event)
        result = sut.get_next_event()
        stub_screen_texture_creation.reverse()
        if check_destroy:
            call = stub_screen_texture_creation.pop()
            assert call['function'] == RSDL.DestroyTexture
            assert call['is_last_created_texture'] == assert_destroyed_last
        call = stub_screen_texture_creation.pop()
        assert call['function'] == RSDL.CreateTexture
        assert call['width'] == width
        assert call['height'] == height
    assert_updated_metrics(300, 200, False)
    assert_updated_metrics(1024, 768, True)
