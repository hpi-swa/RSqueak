import py
from spyvm import squeakimage, model, constants
from spyvm import interpreter, shadow, objspace
from spyvm.test import test_miniimage as testhelper
from spyvm.test.test_miniimage import perform, w

testhelper.setup_module(testhelper, filename='bootstrapped.image')

def test_symbol_asSymbol():
    w_result = perform(testhelper.image.w_asSymbol, "asSymbol")
    assert w_result is testhelper.image.w_asSymbol

def test_create_new_symbol():
    w_result = perform(w("someString"), "asSymbol")
    assert w_result is not None
    assert w_result.as_string() == "someString"

def test_retrieve_symbol():
    py.test.skip("This implementation is based on a working allInstancesDo -> implement primitive 77 and such")
    """asSymbol
    "This is the only place that new Symbols are created. A Symbol is created 
    if and only if there is not already a Symbol with its contents in existance."
    Symbol
        allInstancesDo: [ :sym |
            self = sym
                ifTrue: [ ^ sym ] ].
    ^ (Symbol basicNew: self size) initFrom: self"""
    w_result = perform(w("someString"), "asSymbol")
    w_anotherSymbol = perform(w("someString"), "asSymbol")
    assert w_result is w_anotherSymbol


def test_all_pointers_are_valid():
    testhelper.test_all_pointers_are_valid()