import py
from spyvm import squeakimage, model, constants
from spyvm import interpreter, shadow, objspace
from spyvm.test import test_miniimage as testhelper
from spyvm.test.test_miniimage import perform, w

testhelper.setup_module(testhelper, filename='bootstrapped.image')

def test_retrieve_symbol():
    py.test.skip("This method will fail, because the bytecodes are not adjusted for blockclosures, yet.")
    perform(testhelper.image.w_asSymbol, "asSymbol")

def test_create_new_symbol():
    py.test.skip("This method is based upon the above.")
    #import pdb; pdb.set_trace()
    w_result = perform(w("someString"), "asSymbol")
    assert w_result is not None
    assert w_result.as_string() is "someString"
    

#def test_hazelnut():
#    from spyvm.test import test_miniimage
#    setup_module(test_miniimage, filename='bootstrapped.image')
#    test_miniimage.test_all_pointers_are_valid()
#    test_miniimage.test_become()
    #test_miniimage.test_special_classes0()
