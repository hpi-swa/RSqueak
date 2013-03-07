import sys
import os

from rpython.rlib.streamio import open_file_as_stream
from rpython.rlib import jit

from spyvm import model, interpreter, squeakimage, objspace, wrapper, model
from spyvm.tool.analyseimage import create_image


def _run_benchmark(interp, number, benchmark):
    w_object = model.W_SmallInteger(number)
    try:
        interp.perform(w_object, benchmark)
    except interpreter.ReturnFromTopLevel, e:
        w_result = e.object
        assert isinstance(w_result, model.W_BytesObject)
        print w_result.as_string()
        return 0
    return -1


def _run_image(interp):
    ap = wrapper.ProcessWrapper(space, wrapper.scheduler(space).active_process())
    w_ctx = ap.suspended_context()
    assert isinstance(w_ctx, model.W_PointersObject)
    ap.store_suspended_context(space.w_nil)
    interp.interpret_with_w_frame(w_ctx)
    return 0


space = objspace.ObjSpace()


def _usage(argv):
    print """
    Usage: %s
          -j|--jit [jitargs]
          -n|--number [smallint]
          -m|--method [benchmark on smallint]
          [image path, default: Squeak.image]
    """ % argv[0]


def _arg_missing(argv, idx, arg):
    if len(argv) == idx + 1:
        raise RuntimeError("Error: missing argument after %s" % arg)


def entry_point(argv):
    idx = 1
    image = None
    number = 0
    benchmark = None

    while idx < len(argv):
        arg = argv[idx]
        if arg in ["-h", "--help"]:
            _usage(argv)
            return 0
        elif arg in ["-j", "--jit"]:
            _arg_missing(argv, idx, arg)
            jitarg = argv[idx + 1]
            idx += 1
            jit.set_user_param(interpreter.Interpreter.jit_driver, jitarg)
        elif arg in ["-n", "--number"]:
            _arg_missing(argv, idx, arg)
            number = int(argv[idx + 1])
            idx += 1
        elif arg in ["-m", "--method"]:
            _arg_missing(argv, idx, arg)
            benchmark = argv[idx + 1]
            idx += 1
        elif image is None:
            image = argv[idx]
        else:
            _usage(argv)
            return -1
        idx += 1

    if image is None:
        image = "Squeak.image"

    try:
        f = open_file_as_stream(image)
    except OSError as e:
        os.write(2, "%s -- %s (LoadError)\n" % (os.strerror(e.errno), image))
        return 1
    try:
        imagedata = f.readall()
    finally:
        f.close()

    image_reader = squeakimage.reader_for_image(space, squeakimage.Stream(data=imagedata))
    image = create_image(space, image_reader)
    interp = interpreter.Interpreter(space, image)
    if benchmark is not None:
        return _run_benchmark(interp, number, benchmark)
    else:
        return _run_image(interp)

# _____ Define and setup target ___


def target(*args):
    return entry_point, None


def jitpolicy(self):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()


if __name__ == "__main__":
    entry_point(sys.argv)
