import sys, time
import os

from rpython.rlib.streamio import open_file_as_stream
from rpython.rlib import jit

from spyvm import model, interpreter, squeakimage, objspace, wrapper,\
    error, shadow
from spyvm.tool.analyseimage import create_image


def _run_benchmark(interp, number, benchmark):
    scheduler = wrapper.scheduler(interp.space)
    w_hpp = scheduler.highest_priority_process()
    w_benchmark_proc = model.W_PointersObject(
        interp.space,
        w_hpp.getclass(interp.space),
        len(w_hpp._vars)
    )

    # XXX: Copied from interpreter >> perform
    w_receiver = interp.space.wrap_int(number)
    w_selector = interp.perform(interp.space.wrap_string(benchmark), "asSymbol")
    w_method = model.W_CompiledMethod(header=512)
    w_method.literalatput0(interp.space, 1, w_selector)
    w_method.setbytes([chr(131), chr(0), chr(124)]) #returnTopFromMethod
    s_method = w_method.as_compiledmethod_get_shadow(interp.space)
    s_frame = shadow.MethodContextShadow.make_context(interp.space, s_method, w_receiver, [], None)
    s_frame.push(w_receiver)
    # second variable is suspended context
    w_benchmark_proc.store(space, 1, s_frame.w_self())

    # third variable is priority
    priority = 40
    w_benchmark_proc.store(space, 2, space.wrap_int(priority))

    # make process eligible for scheduling
    wrapper.ProcessWrapper(interp.space, w_benchmark_proc).put_to_sleep()

    t1 = time.time()
    w_result = _run_image(interp)
    t2 = time.time()
    if w_result:
        if isinstance(w_result, model.W_BytesObject):
            print '\n'
            print w_result.as_string()
        print "took %s seconds" % (t2 - t1)
        return 0
    return -1


def _run_image(interp):
    ap = wrapper.ProcessWrapper(space, wrapper.scheduler(space).active_process())
    w_ctx = ap.suspended_context()
    assert isinstance(w_ctx, model.W_PointersObject)
    ap.store_suspended_context(space.w_nil)
    try:
        return interp.interpret_with_w_frame(w_ctx)
    except error.Exit, e:
        print e.msg


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
    path = None
    number = 0
    benchmark = None
    trace = False

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
        elif arg in ["-t", "--trace"]:
            trace = True
        elif path is None:
            path = argv[idx]
        else:
            _usage(argv)
            return -1
        idx += 1

    if path is None:
        path = "Squeak.image"

    try:
        f = open_file_as_stream(path, mode="rb", buffering=0)
    except OSError as e:
        os.write(2, "%s -- %s (LoadError)\n" % (os.strerror(e.errno), path))
        return 1
    try:
        imagedata = f.readall()
    finally:
        f.close()

    image_reader = squeakimage.reader_for_image(space, squeakimage.Stream(data=imagedata))
    image = create_image(space, image_reader)
    interp = interpreter.Interpreter(space, image, image_name=path, trace=trace)
    if benchmark is not None:
        return _run_benchmark(interp, number, benchmark)
    else:
        _run_image(interp)
        return 0

# _____ Define and setup target ___


def target(*args):
    return entry_point, None


def jitpolicy(self):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()


if __name__ == "__main__":
    entry_point(sys.argv)
