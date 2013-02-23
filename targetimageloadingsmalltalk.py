import sys
import os
from spyvm import model, interpreter, primitives, shadow
# from spyvm import classtable
# from spyvm.test.test_interpreter import *
from spyvm import squeakimage
from spyvm import constants

def tinyBenchmarks(space, image_name):
    interp = interpreter.Interpreter(space, image_name=image_name)
    return interp


def run_benchmarks(interp, number, benchmark):
    counter = 0
    w_object = model.W_SmallInteger(number)
    try:
        interp.perform(w_object, "tinyBenchmarks")
    except interpreter.ReturnFromTopLevel, e:
        w_result = e.object

        assert isinstance(w_result, model.W_BytesObject)
        print w_result.as_string()
        return 0
    return -1

from spyvm import objspace
space = objspace.ObjSpace()

def entry_point(argv):
    if len(argv) > 1:
        filename = argv[1]
        if len(argv) > 3:
            number = int(argv[2])
            benchmark = argv[3]
        else:
            number = 0
            benchmark = "tinyBenchmarks"
    else:
        print "usage:", argv[0], "<image name>"
        return -1
    reader = squeakimage.reader_for_image(space, squeakimage.Stream(DummyFile(filename)))
    reader.initialize()
    image = squeakimage.SqueakImage()
    image.from_reader(space, reader)
    interp = interpreter.Interpreter(space, image, filename)
    run_benchmarks(interp, number, benchmark)
    return 0

# _____ Define and setup target ___

def target(*args):
    return entry_point, None

def jitpolicy(self):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()


class DummyFile:
    def __init__(self,filename):
        import os
        fd = os.open(filename, os.O_RDONLY, 0777)
        try:
            content = []
            while 1:
                s = os.read(fd, 4096)
                if not s:
                    break
                content.append(s)
            self.content = "".join(content)
        finally:
            os.close(fd)
    def read(self):
        return self.content
    def close(self):
        pass

if __name__ == "__main__":
    entry_point(sys.argv)
