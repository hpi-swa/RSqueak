import os, sys
from spyvm import model, interpreter, primitives, shadow, constants
from spyvm.tool.analyseimage import create_squeakimage, create_testimage

from rpython.jit.codewriter.policy import JitPolicy

# This loads the whole mini.image in advance.  At run-time,
# it executes the tinyBenchmark.  In this way we get an RPython
# "image" frozen into the executable, mmap'ed by the OS from
# there and loaded lazily when needed :-)


# XXX this only compiles if sys.recursionlimit is high enough!
# On non-Linux platforms I don't know if there is enough stack to
# compile...
#sys.setrecursionlimit(100000)

def setup():
    from spyvm import objspace
    space = objspace.ObjSpace()
    image = create_testimage(space)
    interp = interpreter.Interpreter(space, image)
    w_selector = interp.perform(space.wrap_string("loopTest"), "asSymbol")
    w_object = model.W_SmallInteger(0)
    s_class = w_object.shadow_of_my_class(space)
    s_method = s_class.lookup(w_selector)
    s_frame = s_method.create_frame(space, w_object, [])
    return interp, s_frame

interp, s_frame = setup()

def entry_point(argv):
    try:
        interp.loop(s_frame.w_self())
    except interpreter.ReturnFromTopLevel, e:
        w_result = e.object
        assert isinstance(w_result, model.W_BytesObject)
        print w_result.as_string()
    return 0


# _____ Define and setup target ___

def target(*args):
    return entry_point, None

def jitpolicy(driver):
    return JitPolicy()

if __name__ == "__main__":
    entry_point(sys.argv)
