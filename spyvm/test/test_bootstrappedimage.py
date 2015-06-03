import py
from .util import read_image, copy_to_module, cleanup_module, very_slow_test

def setup_module():
    space, interp, image, reader = read_image("bootstrapped.image")
    w = space.w
    perform = interp.perform
    copy_to_module(locals(), __name__)

def teardown_module():
    cleanup_module(__name__)

def test_symbol_asSymbol():
    w_result = perform(image.w_asSymbol, "asSymbol")
    assert w_result is image.w_asSymbol

@very_slow_test
def test_retrieve_symbol():
    space.initialize_class(space.w_String, interp)
    w_result = perform(w("someString"), "asSymbol")
    assert w_result.unwrap_string(None) == "someString"
    w_anotherSymbol = perform(w("someString"), "asSymbol")
    assert w_result is w_anotherSymbol

def test_all_pointers_are_valid():
    from test_miniimage import _test_all_pointers_are_valid
    _test_all_pointers_are_valid(reader)
