import sys, time
import os

from rpython.rlib.streamio import open_file_as_stream
from rpython.rlib import jit

from spyvm import model, interpreter, squeakimage, objspace, wrapper,\
    error, shadow
from spyvm.tool.analyseimage import create_image
from spyvm.interpreter_proxy import VirtualMachine


def _run_benchmark(interp, number, benchmark, arg):
    from spyvm.plugins.vmdebugging import stop_ui_process
    stop_ui_process()

    scheduler = wrapper.scheduler(interp.space)
    w_hpp = scheduler.active_process()
    if space.unwrap_int(scheduler.active_process().fetch(space, 2)) > space.unwrap_int(w_hpp.fetch(space, 2)):
        w_hpp = scheduler.active_process()
    assert isinstance(w_hpp, model.W_PointersObject)
    w_benchmark_proc = model.W_PointersObject(
        interp.space,
        w_hpp.getclass(interp.space),
        len(w_hpp._vars)
    )

    s_frame = context_for(interp, number, benchmark, arg)
    # second variable is suspended context
    w_benchmark_proc.store(space, 1, s_frame.w_self())

    # third variable is priority
    priority = space.unwrap_int(w_hpp.fetch(space, 2)) / 2 + 1
    # Priorities below 10 are not allowed in newer versions of Squeak.
    if interp.image.version.has_closures:
        priority = max(11, priority)
    else:
        priority = 7
    w_benchmark_proc.store(space, 2, space.wrap_int(priority))

    # make process eligible for scheduling
    wrapper.ProcessWrapper(interp.space, w_benchmark_proc).put_to_sleep()

    t1 = time.time()
    w_result = _run_image(interp)
    t2 = time.time()
    if w_result:
        if isinstance(w_result, model.W_BytesObject):
            print w_result.as_string().replace('\r', '\n')
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

def _run_code(interp, code, as_benchmark=False):
    import time
    selector = "codeTest%d" % int(time.time())
    try:
        w_result = interp.perform(
            interp.space.w_SmallInteger,
            "compile:classified:notifying:",
            space.wrap_string("%s\r\n%s" % (selector, code)),
            space.wrap_string("spy-run-code"),
            space.w_nil
        )
    except interpreter.ReturnFromTopLevel, e:
        print e.object
        return 1
    except error.Exit, e:
        print e.msg
        return 1

    if not as_benchmark:
        try:
            w_result = interp.perform(space.wrap_int(0), selector)
        except interpreter.ReturnFromTopLevel, e:
            print e.object
            return 1
        except error.Exit, e:
            print e.msg
            return 1
        if w_result:
            if isinstance(w_result, model.W_BytesObject):
                print w_result.as_string().replace('\r', '\n')
            else:
                print w_result.as_repr_string().replace('\r', '\n')
        return 0
    else:
        return _run_benchmark(interp, 0, selector, "")


space = objspace.ObjSpace()

def context_for(interp, number, benchmark, stringarg):
    # XXX: Copied from interpreter >> perform
    argcount = 0 if stringarg == "" else 1
    w_receiver = interp.space.wrap_int(number)
    w_selector = interp.perform(interp.space.wrap_string(benchmark), "asSymbol")
    w_method = model.W_CompiledMethod(header=512)
    w_method.literalatput0(interp.space, 1, w_selector)
    w_method.setbytes([chr(131), chr(argcount << 5), chr(124)]) #returnTopFromMethod
    s_method = w_method.as_compiledmethod_get_shadow(interp.space)
    s_frame = shadow.MethodContextShadow.make_context(interp.space, s_method, w_receiver, [], None)
    s_frame.push(w_receiver)
    if not stringarg == "":
        s_frame.push(interp.space.wrap_string(stringarg))
    return s_frame

def _usage(argv):
    print """
    Usage: %s
          -j|--jit [jitargs]
          -n|--number [smallint, default: 0]
          -m|--method [benchmark on smallint]
          -a|--arg [string argument to #method]
          -r|--run [code string]
          -b|--benchmark [code string]
          -p|--poll_events
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
    evented = True
    stringarg = ""
    code = None
    as_benchmark = False

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
        elif arg in ["-p", "--poll_events"]:
            evented = False
        elif arg in ["-a", "--arg"]:
            _arg_missing(argv, idx, arg)
            stringarg = argv[idx + 1]
            idx += 1
        elif arg in ["-r", "--run"]:
            _arg_missing(argv, idx, arg)
            code = argv[idx + 1]
            as_benchmark = False
            idx += 1
        elif arg in ["-b", "--benchmark"]:
            _arg_missing(argv, idx, arg)
            code = argv[idx + 1]
            as_benchmark = True
            idx += 1
        elif path is None:
            path = argv[idx]
        else:
            _usage(argv)
            return -1
        idx += 1

    if path is None:
        path = "Squeak.image"

    path = os.path.abspath(path)
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
    interp = interpreter.Interpreter(space, image, image_name=path, trace=trace, evented=evented)
    space.runtime_setup(argv[0])
    if benchmark is not None:
        return _run_benchmark(interp, number, benchmark, stringarg)
    elif code is not None:
        return _run_code(interp, code, as_benchmark=as_benchmark)
    else:
        _run_image(interp)
        return 0

# _____ Define and setup target ___


def target(driver, *args):
    # driver.config.translation.gc = "stmgc"
    # driver.config.translation.gcrootfinder = "stm"
    from rpython.rlib import rgc
    if hasattr(rgc, "stm_is_enabled"):
        driver.config.translation.stm = True
        driver.config.translation.thread = True
    return entry_point, None


def jitpolicy(self):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()


if __name__ == "__main__":
    entry_point(sys.argv)
