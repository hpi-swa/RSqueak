#! /usr/bin/env python
import sys
import time
import os

from rsqueakvm import interpreter, squeakimage, objspace, wrapper, error
from rsqueakvm.model.pointers import W_PointersObject
from rsqueakvm.model.variable import W_BytesObject
from rsqueakvm.plugins.simulation import SIMULATE_PRIMITIVE_SELECTOR
from rsqueakvm.util import system

from rpython.jit.codewriter.policy import JitPolicy
from rpython.rlib import jit, rpath, objectmodel, streamio


sys.setrecursionlimit(15000)

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
            --no-highdpi           - Disable High-DPI support (default: on).
            --software-renderer    - Use software renderer (default: off).
            --no-display           - Use dummy display (default: off).
            -h|--help              - Output this message and exit.
            -v|--version           - Print version info and exit.

          Execution:
            -r|--run <code>  - Code will be compiled and executed in
                               headless mode, result printed.
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
            -p|--poll          - Actively poll for events. Try this if the
                                 image is not responding well.
            -i|--no-interrupts - Disable timer interrupt.
                                 Disables non-cooperative scheduling.
            -S|--no-storage    - Disable specialized storage strategies.
                                 Always use generic ListStrategy. Probably slower.
            --hacks            - Enable Spy hacks. Set display color depth to 8
            --use-plugins      - Directs named primitives to go to the native
                                 Squeak plugins, which must be in the dynamic
                                 linker path.

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

prebuilt_space = objspace.ObjSpace()

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
            elif arg == "--no-highdpi":
                self.space.highdpi.deactivate()
            elif arg == "--software-renderer":
                self.space.software_renderer.activate()
            elif arg == "--no-display":
                self.space.no_display.activate()
            # Execution
            elif arg in ["-r", "--run"]:
                self.code, idx = get_parameter(argv, idx, arg)
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
                from rsqueakvm.plugins.v_m_debugging import stop_ui_process
                stop_ui_process()
            elif arg in ["--simulate-numeric-primitives"]:
                self.space.simulate_numeric_primitives.activate()
            # Other
            elif arg in ["-j", "--jit"]:
                jitarg, idx = get_parameter(argv, idx, arg)
                jit.set_user_param(interpreter.Interpreter.jit_driver, jitarg)
            elif arg in ["--reader-jit-args"]:
                jitarg, idx = get_parameter(argv, idx, arg)
                squeakimage.set_reader_user_param(jitarg)
            elif arg in ["-p", "--poll"]:
                self.poll = True
            elif arg in ["-i", "--no-interrupts"]:
                self.interrupts = False
            elif arg in ["-S", "--no-storage"]:
                self.space.strategy_factory.no_specialized_storage.activate()
            elif arg in ["--hacks"]:
                self.space.run_spy_hacks.activate()
            elif arg in ["--use-plugins"]:
                self.space.use_plugins.activate()
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
            if not system.IS_ARM and (system.IS_WINDOWS or system.IS_LINUX or system.IS_DARWIN):
                from rsqueakvm.util import dialog
                path = dialog.get_file()
            else:
                path = "Squeak.image"
        self.path = rpath.rabspath(path)

    def sanitize(self):
        self.ensure_path()
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
    jit.set_param(None, "trace_limit", 16000)
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

    if cfg.code or cfg.selector:
        # Mark headless mode when running code or selector
        argv.append('-headless')

    # Load & prepare image and environment
    image = squeakimage.ImageReader(space, stream, cfg.log_image_loading).create_image()
    interp = interpreter.Interpreter(space, image,
                trace=cfg.trace, trace_important=cfg.trace_important,
                evented=not cfg.poll, interrupts=cfg.interrupts)
    space.runtime_setup(cfg.exepath, argv, cfg.path, cfg.extra_arguments_idx)

    interp.populate_remaining_special_objects()
    print_error("") # Line break after image-loading characters

    # Create context to be executed
    if cfg.code or cfg.selector:
        if not cfg.have_number:
            w_receiver = space.w_nil
        else:
            w_receiver = space.wrap_int(cfg.number)
        if cfg.code:
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
    with objspace.ForceHeadless(space):
        if interp.image.version.is_modern:
            w_result = interp.perform(
                w_receiver_class,
                "compile:classified:withStamp:notifying:logSource:",
                w_arguments = [space.wrap_string("%s\r\n%s" % (selector, code)),
                               space.wrap_string("spy-run-code"),
                               space.w_nil,
                               space.w_nil,
                               space.w_false]
            )
        else:
            w_result = interp.perform(
                w_receiver_class,
                "compile:classified:notifying:",
                w_arguments = [space.wrap_string("%s\r\n%s" % (selector, code)),
                               space.wrap_string("spy-run-code"),
                               space.w_nil]
            )
        # TODO - is this expected in every image?
        if not isinstance(w_result, W_BytesObject) or space.unwrap_string(w_result) != selector:
            raise error.Exit("Unexpected compilation result (probably failed to compile): %s" % result_string(w_result))
    space.suppress_process_switch.deactivate()

    w_receiver_class.as_class_get_shadow(space).s_methoddict().sync_method_cache()
    return selector

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

# _____ Target and Main _____

def target(driver, *args):
    driver.exe_name = "rsqueak"
    return safe_entry_point, None

def jitpolicy(self):
    return JitPolicy()

if __name__ == '__main__':
    assert not objectmodel.we_are_translated()
    from rpython.translator.driver import TranslationDriver
    f, _ = target(TranslationDriver(), sys.argv)
    try:
        sys.exit(f(sys.argv))
    except SystemExit:
        pass
    except:
        if hasattr(sys, 'ps1') or not sys.stderr.isatty():
            # we are in interactive mode or we don't have a tty-like
            # device, so we call the default hook
            sys.__excepthook__(type, value, tb)
        else:
            import pdb, traceback
            _type, value, tb = sys.exc_info()
            traceback.print_exception(_type, value, tb)
            pdb.post_mortem(tb)
