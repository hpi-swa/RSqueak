#! /usr/bin/env python
import sys, time, os

from rpython.rlib import jit, rpath, objectmodel
from spyvm import model, interpreter, squeakimage, objspace, wrapper, error

def _usage(argv):
    print """
    Usage: %s <path> [-r|-m|-h] [-naPu] [-jpiS] [-tTslLE]
            <path> - image path (default: Squeak.image)

          Execution mode:
            (no flags)             - Image will be normally opened.
            -r|--run <code>        - Code will be compiled and executed in headless mode, result printed.
            -m|--method <selector> - Selector will be sent to a SmallInteger in headless mode, result printed.
            -h|--help              - Output this and exit.

          Execution parameters:
            -n|--num <int> - Only with -m or -r. SmallInteger to be used as receiver (default: nil).
            -a|--arg <arg> - Only with -m. Will be used as single String argument.
            -P|--process   - Only with -m or -r. Disable headless mode.
                             A high-priority Process for the new context will be created.
                             The last active Process in the image will be started,
                             but then quickly switch to the new synthetic high-prio Process.
                             By default, in headless mode, the active process in the image will be ignored,
                             and the image window will probably not open (good for benchmarking).
                             Headless mode also influences error reporting.
                             Without -r or -m, headless mode is always disabled.
            -u             - Only with -m or -r. Try to stop UI-process at startup. Can help benchmarking.

          Other parameters:
            -j|--jit <jitargs> - jitargs will be passed to the jit configuration.
            -p|--poll          - Actively poll for events. Try this if the image is not responding well.
            -i|--no-interrupts - Disable timer interrupt. Disables non-cooperative scheduling.
            -S                 - Disable specialized storage strategies; always use generic ListStorage
            --hacks            - Enable Spy hacks. Set display color depth to 8.
            
          Logging parameters:
            -t|--trace                 - Output a trace of each message, primitive, return value and process switch.
            -T                         - Trace important events (Process switch, stack overflow, sender chain manipulation)
            -s|--safe-trace            - If tracing is active, omit printing contents of BytesObjects
            -l|--storage-log           - Output a log of storage operations.
            -L|--storage-log-aggregate - Output an aggregated storage log at the end of execution.

    """ % argv[0]

def get_parameter(argv, idx, arg):
    if len(argv) < idx + 1:
        raise error.Exit("Missing argument after %s" % arg)
    return argv[idx], idx + 1
    
def get_int_parameter(argv, idx, arg):
    param, idx = get_parameter(argv, idx, arg)
    try:
        result = int(param)
    except ValueError, e:
        raise error.Exit("Non-int argument after %s" % arg)
    return result, idx
    
def print_error(str):
    os.write(2, str + os.linesep)
    
prebuilt_space = objspace.ObjSpace()

def safe_entry_point(argv):
    try:
        return entry_point(argv)
    except error.Exit, e:
        print_error("Exited: %s" % e.msg)
        return -1
    except error.SmalltalkException, e:
        print_error("Unhandled %s. Message: %s" % (e.exception_type, e.msg))
        return -1
    except BaseException, e:
        print_error("Exception: %s" % str(e))
        if not objectmodel.we_are_translated():
            import traceback
            traceback.print_exc()
        return -1
    finally:
        prebuilt_space.strategy_factory.logger.print_aggregated_log()

def entry_point(argv):
    # == Main execution parameters
    path = None
    selector = None
    code = ""
    number = 0
    have_number = False
    stringarg = None
    headless = True
    # == Other parameters
    poll = False
    interrupts = True
    trace = False
    trace_important = False
    
    space = prebuilt_space
    idx = 1
    try:
        while idx < len(argv):
            arg = argv[idx]
            idx += 1
            if arg in ["-h", "--help"]:
                _usage(argv)
                return 0
            elif arg in ["-j", "--jit"]:
                jitarg, idx = get_parameter(argv, idx, arg)
                jit.set_user_param(interpreter.Interpreter.jit_driver, jitarg)
            elif arg in ["-n", "--number"]:
                number, idx = get_int_parameter(argv, idx, arg)
                have_number = True
            elif arg in ["-m", "--method"]:
                selector, idx = get_parameter(argv, idx, arg)
            elif arg in ["-t", "--trace"]:
                trace = True
            elif arg in ["-T"]:
                trace_important = True
            elif arg in ["-s", "--safe-trace"]:
                space.omit_printing_raw_bytes.activate()
            elif arg in ["-p", "--poll"]:
                poll = True
            elif arg in ["-a", "--arg"]:
                stringarg, idx = get_parameter(argv, idx, arg)
            elif arg in ["-r", "--run"]:
                code, idx = get_parameter(argv, idx, arg)
            elif arg in ["-i", "--no-interrupts"]:
                interrupts = False
            elif arg in ["-P", "--process"]:
                headless = False
            elif arg in ["--hacks"]:
                space.run_spy_hacks.activate()
            elif arg in ["-S"]:
                space.strategy_factory.no_specialized_storage.activate()
            elif arg in ["-u"]:
                from spyvm.plugins.vmdebugging import stop_ui_process
                stop_ui_process()
            elif arg in ["-l", "--storage-log"]:
                space.strategy_factory.logger.activate()
            elif arg in ["-L", "--storage-log-aggregate"]:
                space.strategy_factory.logger.activate(aggregate=True)
            elif path is None:
                path = arg
            else:
                _usage(argv)
                return -1
        
        if path is None:
            path = "Squeak.image"
        if code and selector:
            raise error.Exit("Cannot handle both -r and -m.")
    except error.Exit as e:
        print_error("Parameter error: %s" % e.msg)
        return 1
    
    path = rpath.rabspath(path)
    try:
        stream = squeakimage.Stream(filename=path)
    except OSError as e:
        print_error("%s -- %s (LoadError)" % (os.strerror(e.errno), path))
        return 1
    
    # Load & prepare image and environment
    image = squeakimage.ImageReader(space, stream).create_image()
    interp = interpreter.Interpreter(space, image,
                trace=trace, trace_important=trace_important,
                evented=not poll, interrupts=interrupts)
    space.runtime_setup(argv[0], path)
    interp.populate_remaining_special_objects()
    print_error("") # Line break after image-loading characters
    
    # Create context to be executed
    if code or selector:
        if not have_number:
            w_receiver = space.w_nil
        else:
            w_receiver = space.wrap_int(number)
        if code:
            selector = compile_code(interp, w_receiver, code)
        s_frame = create_context(interp, w_receiver, selector, stringarg)
        if headless:
            space.headless.activate()
            context = s_frame
        else:
            create_process(interp, s_frame)
            context = active_context(space)
    else:
        context = active_context(space)
    
    w_result = execute_context(interp, context)
    print result_string(w_result)
    return 0

def result_string(w_result):
    # This will also print contents of strings/symbols/numbers
    if not w_result:
        return ""
    return w_result.as_repr_string().replace('\r', '\n')

def compile_code(interp, w_receiver, code):
    selector = "DoIt%d" % int(time.time())
    space = interp.space
    w_receiver_class = w_receiver.getclass(space)
    
    # The suppress_process_switch flag is a hack/workaround to enable compiling code
    # before having initialized the image cleanly. The problem is that the TimingSemaphore is not yet
    # registered (primitive 136 not called), so the idle process will never be left once it is entered.
    # TODO - Find a way to cleanly initialize the image, without executing the active_context of the image.
    # Instead, we want to execute our own context. Then remove this flag (and all references to it)
    space.suppress_process_switch.activate()
    
    w_result = interp.perform(
        w_receiver_class,
        "compile:classified:notifying:",
        w_arguments = [space.wrap_string("%s\r\n%s" % (selector, code)),
        space.wrap_string("spy-run-code"),
        space.w_nil]
    )
    # TODO - is this expected in every image?
    if not isinstance(w_result, model.W_BytesObject) or w_result.as_string() != selector:
        raise error.Exit("Unexpected compilation result (probably failed to compile): %s" % result_string(w_result))
    space.suppress_process_switch.deactivate()
    
    w_receiver_class.as_class_get_shadow(space).s_methoddict().sync_method_cache()
    return selector

def create_context(interp, w_receiver, selector, stringarg):
    args = []
    if stringarg:
        args.append(interp.space.wrap_string(stringarg))
    return interp.create_toplevel_context(w_receiver, selector, w_arguments = args)

def create_process(interp, s_frame):
    space = interp.space
    w_active_process = wrapper.scheduler(space).active_process()
    assert isinstance(w_active_process, model.W_PointersObject)
    w_benchmark_proc = model.W_PointersObject(
        space, w_active_process.getclass(space), w_active_process.size()
    )
    if interp.image.version.has_closures:
        # Priorities below 10 are not allowed in newer versions of Squeak.
        active_priority = space.unwrap_int(w_active_process.fetch(space, 2))
        priority = active_priority / 2 + 1
        priority = max(11, priority)
    else:
        priority = 7
    w_benchmark_proc.store(space, 1, s_frame.w_self())
    w_benchmark_proc.store(space, 2, space.wrap_int(priority))

    # Make process eligible for scheduling
    wrapper.ProcessWrapper(space, w_benchmark_proc).put_to_sleep()

def active_context(space):
    w_active_process = wrapper.scheduler(space).active_process()
    active_process = wrapper.ProcessWrapper(space, w_active_process)
    w_active_context = active_process.suspended_context()
    assert isinstance(w_active_context, model.W_PointersObject)
    active_process.store_suspended_context(space.w_nil)
    return w_active_context.as_context_get_shadow(space)

def execute_context(interp, s_frame):
    return interp.interpret_toplevel(s_frame.w_self())

# _____ Target and Main _____

def target(driver, *args):
    # driver.config.translation.gc = "stmgc"
    # driver.config.translation.gcrootfinder = "stm"
    from rpython.rlib import rgc
    if hasattr(rgc, "stm_is_enabled"):
        driver.config.translation.stm = True
        driver.config.translation.thread = True
    driver.exe_name = "rsqueak"
    return safe_entry_point, None

def jitpolicy(self):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()

if __name__ == "__main__":
    safe_entry_point(sys.argv)
