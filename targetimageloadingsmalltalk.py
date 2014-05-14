#! /usr/bin/env python
import sys, time
import os

from rpython.rlib.streamio import open_file_as_stream
from rpython.rlib import jit, rpath

from spyvm import model, interpreter, squeakimage, objspace, wrapper,\
    error, shadow, storage_statistics, constants
from spyvm.tool.analyseimage import create_image
from spyvm.interpreter_proxy import VirtualMachine

def print_result(w_result):
    # This will also print contents of strings/symbols/numbers
    print w_result.as_repr_string().replace('\r', '\n')

def _run_benchmark(interp, number, benchmark, arg):
    from spyvm.plugins.vmdebugging import stop_ui_process
    stop_ui_process()

    space = interp.space
    scheduler = wrapper.scheduler(space)
    w_hpp = scheduler.active_process()
    if space.unwrap_int(scheduler.active_process().fetch(space, 2)) > space.unwrap_int(w_hpp.fetch(space, 2)):
        w_hpp = scheduler.active_process()
    assert isinstance(w_hpp, model.W_PointersObject)
    w_benchmark_proc = model.W_PointersObject(
        space,
        w_hpp.getclass(space),
        w_hpp.size()
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
    wrapper.ProcessWrapper(space, w_benchmark_proc).put_to_sleep()

    t1 = time.time()
    w_result = _run_image(interp)
    t2 = time.time()
    if w_result:
        print_result(w_result)
        print "took %s seconds" % (t2 - t1)
        return 0
    return -1

def _run_image(interp):
    space = interp.space
    ap = wrapper.ProcessWrapper(space, wrapper.scheduler(space).active_process())
    w_ctx = ap.suspended_context()
    assert isinstance(w_ctx, model.W_PointersObject)
    ap.store_suspended_context(space.w_nil)
    try:
        return interp.interpret_toplevel(w_ctx)
    except error.Exit, e:
        print e.msg

def _run_code(interp, code, as_benchmark=False):
    import time
    selector = "DoIt%d" % int(time.time())
    space = interp.space
    w_receiver = space.w_nil
    w_receiver_class = w_receiver.getclass(space)
    try:
        w_result = interp.perform(
            w_receiver_class,
            "compile:classified:notifying:",
            space.wrap_string("%s\r\n%s" % (selector, code)),
            space.wrap_string("spy-run-code"),
            space.w_nil
        )
        w_receiver_class.as_class_get_shadow(space).s_methoddict().sync_method_cache()
    except interpreter.ReturnFromTopLevel, e:
        print e.object
        return 1
    except error.Exit, e:
        print e.msg
        return 1
    
    if not as_benchmark:
        try:
            w_result = interp.perform(w_receiver, selector)
        except interpreter.ReturnFromTopLevel, e:
            print e.object
            return 1
        except error.Exit, e:
            print e.msg
            return 1
        if w_result:
            print_result(w_result)
        return 0
    else:
        return _run_benchmark(interp, 0, selector, "")

def context_for(interp, number, benchmark, stringarg):
    # XXX: Copied from interpreter >> perform
    space = interp.space
    argcount = 0 if stringarg == "" else 1
    w_receiver = space.wrap_int(number)
    w_selector = interp.perform(space.wrap_string(benchmark), "asSymbol")
    w_method = model.W_CompiledMethod(space, header=512)
    w_method.literalatput0(space, 1, w_selector)
    w_method.setbytes([chr(131), chr(argcount << 5), chr(124)]) #returnTopFromMethodBytecodeBytecode
    s_frame = shadow.MethodContextShadow(space, None, w_method, w_receiver, [])
    s_frame.push(w_receiver)
    if not stringarg == "":
        s_frame.push(space.wrap_string(stringarg))
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
          -ni|--no-interrupts
          -d|--max-stack-depth [number, default %d, <= 0 disables stack protection]
          --strategy-log
          --strategy-stats
          --strategy-stats-dot
          --strategy-stats-details
          [image path, default: Squeak.image]
    """ % (argv[0], constants.MAX_LOOP_DEPTH)

def _arg_missing(argv, idx, arg):
    if len(argv) == idx + 1:
        raise RuntimeError("Error: missing argument after %s" % arg)

prebuilt_space = objspace.ObjSpace()

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
    max_stack_depth = constants.MAX_LOOP_DEPTH
    interrupts = True
    
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
        elif arg in ["-ni", "--no-interrupts"]:
            interrupts = False
        elif arg in ["-d", "--max-stack-depth"]:
            _arg_missing(argv, idx, arg)
            max_stack_depth = int(argv[idx + 1])
            idx += 1
        elif arg == "--strategy-log":
            storage_statistics.activate_statistics(log=True)
        elif arg == "--strategy-stats":
            storage_statistics.activate_statistics(statistics=True)
        elif arg == "--strategy-stats-dot":
            storage_statistics.activate_statistics(dot=True)
        elif arg == "--strategy-stats-details":
            storage_statistics.activate_statistics(statistics=True, detailed_statistics=True)
        elif path is None:
            path = argv[idx]
        else:
            _usage(argv)
            return -1
        idx += 1

    if path is None:
        path = "Squeak.image"

    path = rpath.rabspath(path)
    try:
        f = open_file_as_stream(path, mode="rb", buffering=0)
        try:
            imagedata = f.readall()
        finally:
            f.close()
    except OSError as e:
        os.write(2, "%s -- %s (LoadError)\n" % (os.strerror(e.errno), path))
        return 1
    
    space = prebuilt_space
    image_reader = squeakimage.reader_for_image(space, squeakimage.Stream(data=imagedata))
    image = create_image(space, image_reader)
    interp = interpreter.Interpreter(space, image, image_name=path,
                trace=trace, evented=evented,
                interrupts=interrupts, max_stack_depth=max_stack_depth)
    space.runtime_setup(argv[0])
    result = 0
    if benchmark is not None:
        result = _run_benchmark(interp, number, benchmark, stringarg)
    elif code is not None:
        result = _run_code(interp, code, as_benchmark=as_benchmark)
    else:
        _run_image(interp)
        result = 0
    storage_statistics.print_statistics()
    return result


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
