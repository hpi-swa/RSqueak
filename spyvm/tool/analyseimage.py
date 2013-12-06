import py
from spyvm import squeakimage
from spyvm import constants
from spyvm import model
from spyvm import interpreter
import sys

image_dir = py.path.local(__file__).dirpath().dirpath().dirpath('images')

mini_image = image_dir.join('mini.image')
minitest_image = image_dir.join('minitest.image')
s45_image = image_dir.join('Squeak4.5-12568.image')

def get_miniimage(space):
    return squeakimage.reader_for_image(space, squeakimage.Stream(mini_image.open(mode="rb")))

def get_minitestimage(space):
    return squeakimage.reader_for_image(space, squeakimage.Stream(minitest_image.open(mode="rb")))

def get_45image(space):
    return squeakimage.reader_for_image(space, squeakimage.Stream(s45_image.open(mode="rb")))

def create_image(space, image_reader):
    image_reader.initialize()

    image = squeakimage.SqueakImage()
    image.from_reader(space, image_reader)
    return image

def create_squeakimage(space):
    return create_image(space, get_miniimage(space))

def create_testimage(space):
    return create_image(space, get_minitestimage(space))

def create_45image(space):
    return create_image(space, get_45image(space))

def printStringsInImage():
    image = create_squeakimage()
    for each in image.objects:
        if isinstance(each,model.W_BytesObject):
          print each.shadow_of_my_class()
          print each.as_string()

def tinyBenchmarks():
    image = create_squeakimage()
    interp = interpreter.Interpreter()

    w_object = model.W_SmallInteger(0)

    # Should get this from w_object
    w_smallint_class = image.special(constants.SO_SMALLINTEGER_CLASS)
    s_class = w_object.shadow_of_my_class()
    #w_method = s_class.lookup("benchFib")
    w_method = s_class.lookup("tinyBenchmarks")

    assert w_method
    w_frame = w_method.create_frame(w_object, [])
    interp.store_w_active_context(w_frame)

    from spyvm.interpreter import BYTECODE_TABLE
    while True:
        try:
            interp.step()
        except interpreter.ReturnFromTopLevel, e:
            print e.object
            return

def test_do():
    #testSelector()
    #printStringsInImage()
    #testDoesNotUnderstand()
    tinyBenchmarks()

if __name__ == '__main__':
    test_do()
