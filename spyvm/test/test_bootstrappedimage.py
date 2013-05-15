import py
from spyvm import squeakimage, model, constants
from spyvm import interpreter, shadow, objspace
from spyvm.test import test_miniimage as tools
from spyvm.test.test_miniimage import perform, w

def setup():
    tools.setup_module(tools, filename='bootstrapped.image')
    test_initialize_string_class()

def find_symbol_in_methoddict_of(string, s_class):
    s_methoddict = s_class.s_methoddict()
    s_methoddict.sync_cache()
    methoddict_w = s_methoddict.methoddict
    for each in methoddict_w.keys():
        if each.as_string() == string:
            return each

def initialize_class(w_class):
    initialize_symbol = find_symbol_in_methoddict_of("initialize",
                        w_class.shadow_of_my_class(tools.space))
    perform(w_class, initialize_symbol)

def test_initialize_string_class():
    #initialize String class, because equality testing requires a class var set.
    initialize_class(w("string").getclass(tools.space))

def test_symbol_asSymbol():
    w_result = perform(tools.image.w_asSymbol, "asSymbol")
    assert w_result is tools.image.w_asSymbol

def test_create_new_symbol():
    py.test.skip("This test takes quite long and is actually included in test_retrieve_symbol.")
    w_result = perform(w("someString"), "asSymbol")
    assert w_result is not None
    assert w_result.as_string() == "someString"

def test_retrieve_symbol():
    """asSymbol
    "This is the only place that new Symbols are created. A Symbol is created
    if and only if there is not already a Symbol with its contents in existance."
    Symbol
        allInstancesDo: [ :sym |
            self = sym
                ifTrue: [ ^ sym ] ].
    ^ (Symbol basicNew: self size) initFrom: self"""
    w_result = perform(w("someString"), "asSymbol")
    assert w_result.as_string() == "someString"
    w_anotherSymbol = perform(w("someString"), "asSymbol")
    assert w_result is w_anotherSymbol

def test_all_pointers_are_valid():
    tools.test_all_pointers_are_valid()
    tools.test_lookup_abs_in_integer()
