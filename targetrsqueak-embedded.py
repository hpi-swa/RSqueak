#! /usr/bin/env python
import sys
from rpython.jit.codewriter.policy import JitPolicy
from spyvm import model, objspace, interpreter, squeakimage

# This loads an image file in advance and includes it in the
# translation-output. At run-time, the defined selector is sent
# to the defined SmallInteger. This way we get an RPython
# "image" frozen into the executable, mmap'ed by the OS from
# there and loaded lazily when needed :-)
# Besides testing etc., this can be used to create standalone
# binaries executing a smalltalk program.

sys.setrecursionlimit(100000)

imagefile = "images/mini.image"
selector = "loopTest"
receiver = 0

def setup():
    space = objspace.ObjSpace()
    stream = squeakimage.Stream(filename=imagefile)
    image = squeakimage.ImageReader(space, stream).create_image()
    interp = interpreter.Interpreter(space, image)
    w_selector = interp.perform(space.wrap_string(selector), "asSymbol")
    w_object = model.W_SmallInteger(receiver)
    s_class = w_object.class_shadow(space)
    w_method = s_class.lookup(w_selector)
    s_frame = w_method.create_frame(space, w_object)
    return interp, s_frame

interp, s_frame = setup()

def entry_point(argv):
    if len(argv) > 1:
        print "This RSqueak VM has an embedded image and ignores all cli-parameters."
    try:
        interp.loop(s_frame.w_self())
    except interpreter.ReturnFromTopLevel, e:
        w_result = e.object
        assert isinstance(w_result, model.W_BytesObject)
        print w_result.as_string()
    return 0


# _____ Define and setup target ___

def target(driver, *args):
    driver.exe_name = "rsqueak-embedded"
    return entry_point, None

def jitpolicy(driver):
    return JitPolicy()

if __name__ == "__main__":
    entry_point(sys.argv)
