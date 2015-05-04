import py, os, math, time
import pytest
from rpython.rlib.rarithmetic import intmask, r_uint
from rpython.rtyper.lltypesystem import lltype, rffi
from rsdl import RSDL
from spyvm import display, key_constants

@pytest.fixture(autouse=True)
def stub_sdl(monkeypatch):
    monkeypatch.setattr(RSDL, "PollEvent", lambda *args: 0)
    monkeypatch.setattr(RSDL, "Init", lambda *args: 0)
    monkeypatch.setattr(RSDL, "EnableUNICODE", lambda *args: 0)
    monkeypatch.setattr(RSDL, "EnableKeyRepeatWithDefaults", lambda *args: 0)
    monkeypatch.setattr(RSDL, "Quit", lambda: 0)

@pytest.fixture
def stub_event(request):
    p_testevent = lltype.malloc(RSDL.Event, flavor='raw')
    def free_testevent():
        lltype.free(p_testevent, flavor='raw')
    request.addfinalizer(free_testevent)
    return p_testevent

@pytest.fixture
def stub_key_event(stub_event, monkeypatch):
    p_testkeyevent = rffi.cast(RSDL.KeyboardEventPtr, stub_event)
    def PollEvent_stub(p_event):
        p_event.c_type = p_testkeyevent.c_type
        p_keyevent = rffi.cast(RSDL.KeyboardEventPtr, p_event)
        p_keyevent.c_keysym.c_sym = p_testkeyevent.c_keysym.c_sym
        p_keyevent.c_keysym.c_unicode = p_testkeyevent.c_keysym.c_unicode
        return 1
    monkeypatch.setattr(RSDL, "PollEvent", PollEvent_stub)
    return p_testkeyevent

@pytest.fixture
def stub_videoresize_event(stub_event, monkeypatch):
    p_test_resize_event = rffi.cast(RSDL.ResizeEventPtr, stub_event)
    rffi.setintfield(p_test_resize_event, 'c_type', RSDL.VIDEORESIZE)
    def PollEvent_stub(p_event):
        p_event.c_type = p_test_resize_event.c_type
        p_resizeevent = rffi.cast(RSDL.ResizeEventPtr, p_event)
        p_resizeevent.c_w = p_test_resize_event.c_w
        p_resizeevent.c_h = p_test_resize_event.c_h
        return 1
    monkeypatch.setattr(RSDL, "PollEvent", PollEvent_stub)
    return p_test_resize_event

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
    return display.SDLDisplay("test")

def assert_keyevent_array(actual, expected_char_code=None,
        expected_press_state=None, expected_modifiers=None):
    assert len(actual) == 8
    assert actual[0] != 0, "event polled unexpectedly"
    assert actual[0] == display.EventTypeKeyboard
    if expected_char_code:
        assert actual[2] == expected_char_code
    if expected_press_state:
        assert actual[3] == expected_press_state
    if expected_modifiers:
        assert actual[4] == expected_modifiers

def test_keypresses(sut, stub_key_event):
    assert_keypress(sut, stub_key_event, RSDL.K_a, ord('a'))
    assert_keypress(sut, stub_key_event, RSDL.K_b, ord('b'))

## jr: Would it be more appropriate to raise a KeyDown event only once when the
##     user actually pushes the key down? Currently, the event is reraised
##     whenever the KeyChar event is reraised because of repeated keys.

def assert_keypress(sut, stub_key_event, sdl_key, char_code):
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYDOWN)
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', sdl_key)
    rffi.setintfield(stub_key_event.c_keysym, 'c_unicode', char_code)
    result = sut.get_next_event()
    assert_keyevent_array(result, char_code, display.EventKeyDown, 0)
    rffi.setintfield(stub_key_event, 'c_type', 0) # don't expect this to appear
    result = sut.get_next_event()
    assert_keyevent_array(result, char_code, display.EventKeyChar, 0)
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYUP)
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', sdl_key)
    rffi.setintfield(stub_key_event.c_keysym, 'c_unicode', char_code)
    result = sut.get_next_event()
    assert_keyevent_array(result, char_code, display.EventKeyUp, 0)

def test_modifiers_do_not_cause_keychar_event(sut, stub_key_event, stub_mod_state):
    def assert_key(key, sdl_mod, char_code, modifiers):
        assert_does_not_generate_keychar_events(sut, stub_key_event, stub_mod_state, key, sdl_mod, char_code, modifiers)
    assert_key(RSDL.K_LSHIFT, RSDL.KMOD_LSHIFT, key_constants.SHIFT, display.ShiftKeyBit)
    assert_key(RSDL.K_RSHIFT, RSDL.KMOD_RSHIFT, key_constants.SHIFT, display.ShiftKeyBit)
    assert_key(RSDL.K_LCTRL, RSDL.KMOD_LCTRL, key_constants.CTRL, display.CtrlKeyBit)
    assert_key(RSDL.K_RCTRL, RSDL.KMOD_RCTRL, key_constants.CTRL, display.CtrlKeyBit)
    assert_key(RSDL.K_LALT, RSDL.KMOD_LALT, key_constants.COMMAND, display.CommandKeyBit)
    assert_key(RSDL.K_RALT, RSDL.KMOD_RALT, key_constants.COMMAND, display.CommandKeyBit)
    assert_key(RSDL.K_LMETA, RSDL.KMOD_NONE, None, None)
    assert_key(RSDL.K_RMETA, RSDL.KMOD_NONE, None, None)
    assert_key(RSDL.K_LSUPER, RSDL.KMOD_NONE, None, None)
    assert_key(RSDL.K_RSUPER, RSDL.KMOD_NONE, None, None)

def assert_does_not_generate_keychar_events(sut, stub_key_event, stub_mod_state, key, sdl_modifier, expected_char_code, expected_modifiers):
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYDOWN)
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', key)
    stub_mod_state.set(sdl_modifier)
    result = sut.get_next_event()
    assert_keyevent_array(result, expected_char_code, display.EventKeyDown, expected_modifiers)
    result = sut.get_next_event()
    assert_keyevent_array(result, expected_char_code, display.EventKeyDown, expected_modifiers)
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYUP)
    stub_mod_state.set(RSDL.KMOD_NONE)
    result = sut.get_next_event()
    assert_keyevent_array(result, expected_char_code, display.EventKeyUp, 0)

def test_movement_keys(sut, stub_key_event):
    def assert_key(key, expected_char_code):
        assert_keypress(sut, stub_key_event, key, expected_char_code)
    # no loop for test output tracability
    assert_key(RSDL.K_LEFT, key_constants.LEFT)
    assert_key(RSDL.K_RIGHT, key_constants.RIGHT)
    assert_key(RSDL.K_UP, key_constants.UP)
    assert_key(RSDL.K_DOWN, key_constants.DOWN)
    assert_key(RSDL.K_HOME, key_constants.HOME)
    assert_key(RSDL.K_END, key_constants.END)
    assert_key(RSDL.K_PAGEUP, key_constants.PAGEUP)
    assert_key(RSDL.K_PAGEDOWN, key_constants.PAGEDOWN)

def test_other_keys(sut, stub_key_event):
    def assert_key(key, expected_char_code):
        assert_keypress(sut, stub_key_event, key, expected_char_code)
    # no loop for test output tracability
    assert_key(RSDL.K_INSERT, key_constants.INSERT)
    assert_key(RSDL.K_RETURN, key_constants.RETURN)
    assert_key(RSDL.K_BREAK, key_constants.BREAK)
    assert_key(RSDL.K_CAPSLOCK, key_constants.CAPSLOCK)
    assert_key(RSDL.K_ESCAPE, key_constants.ESCAPE)
    assert_key(RSDL.K_PRINT, key_constants.PRINT)
    assert_key(RSDL.K_DELETE, key_constants.DELETE)
    assert_key(RSDL.K_NUMLOCK, key_constants.NUMLOCK)
    assert_key(RSDL.K_SCROLLOCK, key_constants.SCROLLOCK)

# see Character class methods (delete, end, home, ...)
# and Editor class >> initializeKeystrokeActions
# char code can be different between keyChar and keyDown events (as we know from SWA)

def test_entering_captital_letters(sut, stub_key_event, stub_mod_state):
    # shift down
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYDOWN)
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', RSDL.K_LSHIFT)
    stub_mod_state.set(RSDL.KMOD_SHIFT)
    result = sut.get_next_event()
    assert_keyevent_array(result, key_constants.SHIFT, display.EventKeyDown, display.ShiftKeyBit)
    # A down
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', RSDL.K_a)
    rffi.setintfield(stub_key_event.c_keysym, 'c_unicode', ord('A'))
    result = sut.get_next_event()
    assert_keyevent_array(result, ord('A'), display.EventKeyDown, display.ShiftKeyBit)
    # A entered
    result = sut.get_next_event()
    assert_keyevent_array(result, ord('A'), display.EventKeyChar, display.ShiftKeyBit)
    # A up
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYUP)
    result = sut.get_next_event()
    assert_keyevent_array(result, ord('A'), display.EventKeyUp, display.ShiftKeyBit)
    # shift up
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', RSDL.K_LSHIFT)
    result = sut.get_next_event()
    assert_keyevent_array(result, key_constants.SHIFT, display.EventKeyUp, 0)

def test_keyboard_chords(sut, stub_key_event, stub_mod_state):
    # CTRL down
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYDOWN)
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', RSDL.K_LCTRL)
    stub_mod_state.set(RSDL.KMOD_CTRL)
    result = sut.get_next_event()
    assert_keyevent_array(result, key_constants.CTRL, display.EventKeyDown, display.CtrlKeyBit)
    # A down
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', RSDL.K_a)
    rffi.setintfield(stub_key_event.c_keysym, 'c_unicode', ord('a'))
    result = sut.get_next_event()
    assert_keyevent_array(result, ord('a'), display.EventKeyDown, display.CtrlKeyBit)
    result = sut.get_next_event()
    assert_keyevent_array(result, ord('a'), display.EventKeyChar, display.CtrlKeyBit)
    # repeat A down
    result = sut.get_next_event()
    assert_keyevent_array(result, ord('a'), display.EventKeyDown, display.CtrlKeyBit)
    result = sut.get_next_event()
    assert_keyevent_array(result, ord('a'), display.EventKeyChar, display.CtrlKeyBit)
    # A up
    rffi.setintfield(stub_key_event, 'c_type', RSDL.KEYUP)
    result = sut.get_next_event()
    assert_keyevent_array(result, ord('a'), display.EventKeyUp, display.CtrlKeyBit)
    # CTRL up
    rffi.setintfield(stub_key_event.c_keysym, 'c_sym', RSDL.K_LCTRL)
    stub_mod_state.set(RSDL.KMOD_NONE)
    result = sut.get_next_event()
    assert_keyevent_array(result, key_constants.CTRL, display.EventKeyUp, 0)
    
@pytest.fixture
def stub_setVideoMode_call(request, monkeypatch):
    p_stub_screen = lltype.malloc(RSDL.Surface, flavor='raw')
    rffi.setintfield(p_stub_screen, 'c_pitch', 0)
    p_stub_format = lltype.malloc(RSDL.PixelFormat, flavor='raw')
    rffi.setintfield(p_stub_format, 'c_BytesPerPixel', 4)
    p_stub_screen.c_format = p_stub_format
    def free_screen_etc():
        lltype.free(p_stub_format, flavor='raw')
        lltype.free(p_stub_screen, flavor='raw')
    request.addfinalizer(free_screen_etc)
    setVideoMode_called_with_args = [None] * 4
    def setVideoMode_stub(w, h, d, flags):
        setVideoMode_called_with_args[:] = [w, h, d, flags]
        return p_stub_screen
    monkeypatch.setattr(RSDL, 'SetVideoMode', setVideoMode_stub)
    return setVideoMode_called_with_args

def test_window_resize_events(sut, stub_videoresize_event, stub_setVideoMode_call):
    def assert_updated_metrics(width, height):
        rffi.setintfield(stub_videoresize_event, 'c_w', width)
        rffi.setintfield(stub_videoresize_event, 'c_h', height)
        result = sut.get_next_event()
        # TODO: decide whether no events or windowmetric events should be raised
        assert sut.width == width
        assert sut.height == height
    assert_updated_metrics(300, 200)
    assert_updated_metrics(1024, 768)

def test_display_resize_on_resize_events(sut, stub_videoresize_event,
        stub_setVideoMode_call, monkeypatch):
    def assert_updated_metrics(width, height):
        rffi.setintfield(stub_videoresize_event, 'c_w', width)
        rffi.setintfield(stub_videoresize_event, 'c_h', height)
        result = sut.get_next_event()
        assert stub_setVideoMode_call[0] == width
        assert stub_setVideoMode_call[1] == height
    assert_updated_metrics(300, 200)
    assert_updated_metrics(1024, 768)
