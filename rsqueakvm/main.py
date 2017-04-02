import os
import time

from rsqueakvm import interpreter, squeakimage, objspace, wrapper, error
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins import PluginRegistry
from rsqueakvm.plugins.simulation import SIMULATE_PRIMITIVE_SELECTOR
from rsqueakvm.util import system

from rpython.rlib import jit, rpath, objectmodel, streamio

# XXX: HACK: We have circular dependencies in some plugins ... :(
PLUGINS_PATCHED = False
if not PLUGINS_PATCHED:
    PLUGINS_PATCHED = True
    [p.patch() for p in PluginRegistry.enabled_plugins]


def _compile_time_version():
    if os.environ.get("APPVEYOR", None):
        return "%s %s (%s)" % (
            os.environ["APPVEYOR_REPO_COMMIT_TIMESTAMP"],
            os.environ["APPVEYOR_REPO_COMMIT"][0:6],
            os.environ.get("APPVEYOR_REPO_TAG_NAME", None) or os.environ["APPVEYOR_REPO_BRANCH"]
        )
    elif os.environ.get("TRAVIS", None):
        import subprocess
        timestamp = subprocess.check_output(["git", "log", "--format=format:\"%ai\"", "-n", "1"])
        return "%s %s (%s)" % (
            timestamp,
            os.environ["TRAVIS_COMMIT"][0:6],
            os.environ.get("TRAVIS_TAG", None) or os.environ["TRAVIS_BRANCH"]
        )
    else:
        import subprocess
        return subprocess.check_output(
            ["git", "log", "--format=format:\"Home-built: %ai %h%d\"", "-n", "1"])

def _compile_git_version():
    if os.environ.get("APPVEYOR", None):
        return os.environ.get("APPVEYOR_REPO_TAG_NAME", None) or os.environ["APPVEYOR_REPO_COMMIT"][0:6]
    else:
        import subprocess
        return subprocess.check_output(
                ["git", "describe", "--tags", "--always"]).strip()

VERSION = _compile_time_version()
GIT_VERSION = _compile_git_version()
BUILD_DATE = "%s +0000" % time.asctime(time.gmtime())

def _usage(argv):
    print """
    Usage: %s [-r|-m|-h] [-naPu] [-jpiS] [-tTslL] <path> [--] [Squeak arguments]
            <path> - image path (default: Squeak.image)
            Squeak arguments are passed on to the Squeak image rather than being processed.

          General:
            --highdpi              - Enable High-DPI support (default: off).
            --software-renderer    - Use software renderer (default: off).
            --no-display           - Use dummy display (default: off).
            -h|--help              - Output this message and exit.
            --silent               - Disable image loading output
            -v|--version           - Print version info and exit.

          Execution:
            -r|--run <code>  - Code will be compiled and executed in
                               headless mode, result printed.
            -rr <code>       - Code will be compiled and executed in
                               headless mode, twice, the second result printed.
                               This is a workaround for making jittests and
                               benchmarking easier, because do-its are not JIT'ed
                               right now, due to the dynamic frame size
                               calculation.
            -m|--method <selector>
                             - Selector will be sent to nil in
                               headless mode, result printed.
            -n|--num <int>   - Only with -m or -r. SmallInteger to be used as
                               receiver (default: nil).
            -a|--arg <arg>   - Only with -m. Will be used as String argument.
            -P|--process     - Only with -m or -r. Disable headless mode.
                               A high-priority Process for the new context will
                               be created. The last active Process in the image
                               will be started, but then quickly switch to the
                               synthetic high-prio Process.
            -u|--stop-ui     - Only with -m or -r. Try to stop UI-process at
                               startup. Can help benchmarking.
            --run-file       - Run the .st file supplied as first argument. No
                               display will be created, but stdin/stdout will
                               be connected so terminal input/output will work.
            --shell          - Stop after loading the image. Any code typed is
                               compiled an run.
            --simulate-numeric-primitives
                             - This flag determines if an attempt is made to run
                               Slang Simulation code for _unimplemented_ numeric
                               primitives. This means that, if an unimplemented
                               numeric primitive is encountered, rather than just
                               failing, we see if the receiver understands
                               %s.
                               If so, this method is called instead of the
                               fallback code.

          Other:
            -j|--jit <jitargs> - jitargs will be passed to the jit config.
            -o|--opt <optargs> - custom optimization heuristics for the RSqueak
                                 interpreter. Comma-separated list of:
                                     max_squeak_unroll_count (default: 1)
                                         How many extra times we should try
                                         to unroll loops.
                                     squeak_unroll_trace_limit (default: 32000)
                                         Trace length after which we should not
                                         unroll any more Squeak loops.
            -p|--poll          - Actively poll for events. Try this if the
                                 image is not responding well.
            -i|--no-interrupts - Disable timer interrupt.
                                 Disables non-cooperative scheduling.
            -S|--no-storage    - Disable specialized storage strategies.
                                 Always use generic ListStrategy. Probably slower.
            -M|--no-maps       - Disable Self-style maps for small fixed pointer
                                 objects and revert to storage strategies.
            --maps-limit <num> - Number of fields an object can have to be
                                 eligible for use with maps.

          Logging:
            -t|--trace       - Output a trace of each message, primitive,
                               return value and process switch.
            -T|--trace-important
                             - Trace important events: Process switch,
                               stack overflow, sender chain manipulation
            -s|--safe-trace  - If tracing is active, omit printing contents of
                               BytesObjects
            -l|--storage-log - Output a log of storage operations.
            -L|--storage-log-aggregate
                             - Output an aggregated storage log at the end of
                               execution.
            --log-image-loading - print bridges/segments information

          Global: (This section is for compatibility with Squeak.ini)
            --ImageFile <path>   - path to the image file
            --WindowTitle <str>  - string to the image file
            --EnableAltF4Quit    - enable Alt F4 to immediately kill the image

          All options that take arguments can be set in an rsqueak.ini file
          located next to the binary. The sections must correspond to the
          sections given here. The options use their long form and the argument
          after the equals sign. Options without arguments can be set to "1" to
          pass them from the INI file.

          About Headless mode:
            When starting the image without -r or -m, the last running Process
            in the image will be executed. This Process will open the image
            window and initialize the environment. In headless mode, this
            Process is ignored; instead, a new context will be created and
            directly executed. When this context returns, the VM terminates.
            In headless mode certain errors make the VM exit directly. This
            should prevent the image from opening a Debugger when the world is
            not visible. Headless mode is good for benchmarking and quick
            experiments.
    """ % (argv[0], SIMULATE_PRIMITIVE_SELECTOR)

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

def make_initial_space():
    prebuilt_space = objspace.ObjSpace()
    if "JitHooks" in system.optional_plugins:
        from rsqueakvm.plugins.vmdebugging.hooks import jitiface
        jitiface.space = prebuilt_space
    return prebuilt_space
prebuilt_space = make_initial_space()

def safe_entry_point(argv):
    try:
        return entry_point(argv)
    except error.CleanExit as e:
        return 0
    except error.Exit as e:
        print_error("Exited: %s" % e.msg)
        return -1
    except error.SmalltalkException as e:
        print_error("Unhandled %s. Message: %s" % (e.exception_type, e.msg))
        return -1
    except BaseException as e:
        print_error("Exception: %s" % str(e))
        if not objectmodel.we_are_translated():
            raise
        return -1
    finally:
        prebuilt_space.strategy_factory.logger.print_aggregated_log()

class Config(object):
    def __init__(self, space, argv):
        self.space = space
        self.exepath = self.find_executable(argv[0])
        self.argv = argv
        self.path = None
        self.got_lone_path = False
        self.selector = None
        self.code = ""
        self.run_file = False
        self.run_twice = False
        self.number = 0
        self.have_number = False
        self.stringarg = None
        self.headless = True
        self.poll = False
        self.interrupts = True
        self.trace = False
        self.trace_important = False
        self.extra_arguments_idx = len(argv)
        self.log_image_loading = False
        self.shell = False
        self.optargs = interpreter.Optargs()

    def parse_args(self, argv, skip_bad=False):
        idx = 1
        while idx < len(argv):
            arg = argv[idx]
            idx += 1
            # General
            if arg in ["-h", "--help"]:
                _usage(argv)
                raise error.CleanExit()
            elif arg in ["-v", "--version"]:
                print "RSqueakVM %s, built on %s" % (VERSION, BUILD_DATE)
                raise error.CleanExit()
            elif arg in ["--git-version"]:
                print GIT_VERSION
                raise error.CleanExit()
            elif arg == "--highdpi":
                self.space.highdpi.activate()
            elif arg == "--software-renderer":
                self.space.software_renderer.activate()
            # -nodisplay (Linux) and -headless (macOS) are used in Cog
            elif arg in ["--no-display", "-nodisplay", "-headless"]:
                self.space.no_display.activate()
            elif arg == "--silent":
                self.space.silent.activate()
            # Execution
            elif arg in ["-r", "--run"]:
                self.code, idx = get_parameter(argv, idx, arg)
            elif arg == "-rr":
                self.code, idx = get_parameter(argv, idx, arg)
                self.run_twice = True
            elif arg in ["--run-file"]:
                self.run_file = True
            elif arg in ["-m", "--method"]:
                self.selector, idx = get_parameter(argv, idx, arg)
            elif arg in ["-n", "--number"]:
                self.number, idx = get_int_parameter(argv, idx, arg)
                self.have_number = True
            elif arg in ["-a", "--arg"]:
                self.stringarg, idx = get_parameter(argv, idx, arg)
            elif arg in ["-P", "--process"]:
                self.headless = False
            elif arg in ["-u", "--stop-ui"]:
                from rsqueakvm.plugins.vm_debugging import stop_ui_process
                stop_ui_process()
            elif arg == "--shell":
                self.shell = True
            elif arg in ["--simulate-numeric-primitives"]:
                self.space.simulate_numeric_primitives.activate()
            # Cog compatibility by skipping single dash args (e.g. -nosound)
            elif len(arg) > 2 and arg[0] == '-' and not arg.startswith('--'):
                pass
            # Other
            elif arg in ["-j", "--jit"]:
                jitarg, idx = get_parameter(argv, idx, arg)
                # Work around TraceLimitTooHigh by setting any trace_limit explicitly
                parts = jitarg.split(",")
                limitidx = -1
                for i, s in enumerate(parts):
                    if "trace_limit" in s:
                        limitidx = i
                        break
                if limitidx >= 0:
                    limit = parts.pop(limitidx)
                    jit.set_param(interpreter.Interpreter.jit_driver, "trace_limit", int(limit.split("=")[1]))
                if len(parts) > 0:
                    jit.set_user_param(interpreter.Interpreter.jit_driver, ",".join(parts))
            elif arg in ["-o", "--opt"]:
                optarg, idx = get_parameter(argv, idx, arg)
                parts = optarg.split(",")
                for part in parts:
                    key, value = part.split("=")
                    if "max_squeak_unroll_count" == key:
                        self.optargs.max_squeak_unroll_count = int(value)
                    elif "squeak_unroll_trace_limit" == key:
                        self.optargs.squeak_unroll_trace_limit == int(value)
                    else:
                        raise Exception("Wrong argument to %s: %s" % (arg, part))
            elif arg in ["-p", "--poll"]:
                self.poll = True
            elif arg in ["-i", "--no-interrupts"]:
                self.interrupts = False
            elif arg in ["-S", "--no-storage"]:
                self.space.strategy_factory.no_specialized_storage.activate()
            elif arg in ["-M", "--no-maps"]:
                self.space.use_maps.deactivate()
            elif arg in ["--maps-limit"]:
                limit, idx = get_int_parameter(argv, idx, arg)
                self.space.maps_limit.set(limit)
            # Logging
            elif arg in ["-t", "--trace"]:
                self.trace = True
            elif arg in ["-T", "--trace-important"]:
                self.trace_important = True
            elif arg in ["-s", "--safe-trace"]:
                self.space.omit_printing_raw_bytes.activate()
            elif arg in ["-l", "--storage-log"]:
                self.space.strategy_factory.logger.activate()
            elif arg in ["-L", "--storage-log-aggregate"]:
                self.space.strategy_factory.logger.activate(aggregate=True)
            elif arg == "--log-image-loading":
                self.log_image_loading = True
            # Global
            elif arg in ["--ImageFile"]:
                self.path, idx = get_parameter(argv, idx, arg)
            elif arg in ["--WindowTitle"]:
                title, idx = get_parameter(argv, idx, arg)
                self.space.title.set(title)
            elif arg in ["--EnableAltF4Quit"]:
                self.space.altf4quit.activate()
            # Default
            elif arg in ["--"]:
                print "Image arguments: %s" % ", ".join(argv[idx:])
                self.extra_arguments_idx = idx
                return
            elif not self.got_lone_path:
                self.path = arg
                self.got_lone_path = True
                # once we got an image argument, we stop processing and pass
                # everything on to the image
                if idx < len(argv):
                    print "Image arguments: %s" % ", ".join(argv[idx:])
                self.extra_arguments_idx = idx
                return
            else:
                _usage(argv)
                raise error.Exit("Invalid argument: %s" % arg)

    def ensure_path(self):
        path = self.path
        if path:
            if os.path.exists(path):
                self.path = rpath.rabspath(path)
                return
            exedir = self.get_exedir()
            if not exedir:
                return
            path = rpath.rjoin(exedir, path)
            if os.path.exists(path):
                self.path = rpath.rabspath(path)
                return
        else:
            for filename in os.listdir(os.getcwd()):
                if filename.startswith("Squeak") and filename.endswith(".image"):
                    path = filename
                    break
        if path is None:
            from rsqueakvm.util.dialog import get_file
            path = get_file()
        self.path = rpath.rabspath(path)

    def sanitize(self):
        self.ensure_path()
        if self.run_file:
            if self.code:
                raise error.Exit("Cannot handle both --run-file and -r.")
            try:
                codefile = self.argv[self.extra_arguments_idx]
            except IndexError:
                raise error.Exit("Cannot handle --run-file without a file argument.")
            try:
                f = streamio.open_file_as_stream(codefile, mode="r", buffering=0)
            except OSError as e:
                raise error.Exit("Cannot read %s" % codefile)
            try:
                source = list(f.readall())
            finally:
                f.close()
            if len(source) == 0:
                raise error.Exit("Empty source file given")
            idx = len(source) - 1
            assert idx > 0
            while source[idx].isspace():
                idx -= 1
            if source[idx] == "!":
                source[idx] = " "
            self.code = "".join(source)
        if self.code and self.selector:
            raise error.Exit("Cannot handle both -r and -m.")

    def find_executable(self, executable):
        if os.sep in executable or (os.name == "nt" and ":" in executable):
            return rpath.rabspath(executable)
        path = os.environ.get("PATH")
        if path:
            for dir in path.split(os.pathsep):
                f = os.path.join(dir, executable)
                if os.path.isfile(f):
                    executable = f
                    break
        return rpath.rabspath(executable)

    def get_exedir(self):
        splitpaths = self.exepath.split(os.sep)
        splitlen = len(splitpaths)
        # tfel: The dance below makes translation work. os.path.dirname breaks :(
        if splitlen > 2:
            splitlen = splitlen - 1
            assert splitlen >= 0
            return os.sep.join(splitpaths[0:splitlen])
        else:
            return

    def init_from_ini(self):
        exedir = self.get_exedir()
        if not exedir:
            return
        inifile = rpath.rjoin(exedir, "rsqueak.ini")
        if os.path.exists(inifile):
            f = streamio.open_file_as_stream(inifile, mode="r", buffering=0)
            try:
                argini = [""]
                line = f.readline().strip()
                while len(line) > 0:
                    if line.startswith("[") or line.startswith(";"):
                        pass
                    elif "=" in line:
                        option, param = line.split("=", 1)
                        option = option.strip()
                        param = param.strip()
                        param = param.strip("\"'")
                        if param == "1":
                            argini += ["--%s" % option]
                        elif param == "0":
                            pass
                        else:
                            argini += ["--%s" % option, param]
                    else:
                        raise error.Exit("Invalid line in INI file: %s" % line)
                    line = f.readline().strip()
                self.parse_args(argini)
            finally:
                f.close()

    def init_from_arguments(self):
        return self.parse_args(self.argv)


def entry_point(argv):
    jit.set_param(None, "trace_limit", 1000000)
    jit.set_param(None, "threshold", 8209) # just above 2**!3, prime
    jit.set_param(None, "function_threshold", 9221) # slightly more than above, also prime
    # == Main execution parameters
    space = prebuilt_space
    cfg = Config(space, argv)

    try:
        cfg.init_from_ini()
        cfg.init_from_arguments()
        cfg.sanitize()
    except error.CleanExit as e:
        return 0
    except error.Exit as e:
        print_error(e.msg)
        return 1

    try:
        stream = squeakimage.Stream(filename=cfg.path)
    except OSError as e:
        print_error("%s -- %s (LoadError)" % (os.strerror(e.errno), cfg.path))
        return 1

    if cfg.code or cfg.selector or cfg.shell:
        # Mark headless mode when running code or selector
        argv.append('-headless')

    # Load & prepare image and environment
    image = squeakimage.ImageReader(space, stream, cfg.log_image_loading).create_image()
    interp = interpreter.Interpreter(space, image,
                trace=cfg.trace, trace_important=cfg.trace_important,
                evented=not cfg.poll, interrupts=cfg.interrupts,
                optargs=cfg.optargs)
    space.runtime_setup(interp, cfg.exepath, argv, cfg.path, cfg.extra_arguments_idx)
    print_error("") # Line break after image-loading characters

    # Create context to be executed
    if cfg.shell:
        from rsqueakvm.util.shell import Shell
        code = cfg.code
        cfg = None
        Shell(interp, space, code=code).run()
        return 0
    elif cfg.code or cfg.selector:
        if not cfg.have_number:
            w_receiver = space.w_nil
        else:
            w_receiver = space.wrap_int(cfg.number)
        if cfg.code:
            if cfg.run_file: # connect the stdio streams
                selector = compile_code(
                    interp, w_receiver, "FileStream startUp: true")
                with objspace.ForceHeadless(space):
                    interp.perform(w_receiver, selector)
            if cfg.run_twice:
                selector = compile_code(interp, w_receiver, cfg.code)
                cfg.selector = compile_code(
                    interp, w_receiver, "^ self %s; %s" % (selector, selector))
            else:
                cfg.selector = compile_code(interp, w_receiver, cfg.code)
        s_frame = create_context(interp, w_receiver, cfg.selector, cfg.stringarg)
        if cfg.headless:
            space.headless.activate()
            context = s_frame
        else:
            create_process(interp, s_frame)
            context = active_context(space)
    else:
        context = active_context(space)

    cfg = None # make sure we free this
    w_result = execute_context(interp, context)
    print result_string(w_result)
    return 0

def result_string(w_result):
    # This will also print contents of strings/symbols/numbers
    if not w_result:
        return ""
    return w_result.as_repr_string().replace('\r', '\n')

def compile_code(interp, w_receiver, code, isclass=False, make_selector=True):
    if make_selector:
        selector = "DoIt%d\r\n" % (int(time.time()) + len(code))
    else:
        selector = ""
    space = interp.space
    if isclass:
        w_receiver_class = w_receiver
    else:
        w_receiver_class = w_receiver.getclass(space)

    # The suppress_process_switch flag is a hack/workaround to enable compiling code
    # before having initialized the image cleanly. The problem is that the TimingSemaphore is not yet
    # registered (primitive 136 not called), so the idle process will never be left once it is entered.
    # TODO - Find a way to cleanly initialize the image, without executing the active_context of the image.
    # Instead, we want to execute our own context. Then remove this flag (and all references to it)
    space.suppress_process_switch.activate()
    with objspace.ForceHeadless(space):
        if interp.image.version.is_modern:
            w_result = interp.perform(
                w_receiver_class,
                "compile:classified:withStamp:notifying:logSource:",
                w_arguments = [space.wrap_string("".join([selector, code])),
                               space.wrap_string("spy-run-code"),
                               space.w_nil,
                               space.w_nil,
                               space.w_false]
            )
        else:
            w_result = interp.perform(
                w_receiver_class,
                "compile:classified:notifying:",
                w_arguments = [space.wrap_string("".join([selector, code])),
                               space.wrap_string("spy-run-code"),
                               space.w_nil]
            )
        # TODO - is this expected in every image?
        if not isinstance(w_result, W_BytesObject):
            raise error.Exit("Unexpected compilation result (probably failed to compile): %s" % result_string(w_result))
    space.suppress_process_switch.deactivate()

    w_receiver_class.as_class_get_shadow(space).s_methoddict().sync_method_cache()
    return selector.strip()

def create_context(interp, w_receiver, selector, stringarg):
    args = []
    if stringarg:
        args.append(interp.space.wrap_string(stringarg))
    return interp.create_toplevel_context(w_receiver, selector=selector, w_arguments=args)

def create_process(interp, s_frame):
    space = interp.space
    w_active_process = wrapper.scheduler(space).active_process()
    assert isinstance(w_active_process, W_PointersObject)
    w_benchmark_proc = W_PointersObject(
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
    assert isinstance(w_active_context, W_PointersObject)
    active_process.store_suspended_context(space.w_nil)
    return w_active_context.as_context_get_shadow(space)

def execute_context(interp, s_frame):
    return interp.interpret_toplevel(s_frame.w_self())
