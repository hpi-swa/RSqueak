#! /usr/bin/env python

import sys, os, platform, subprocess, ConfigParser
from os.path import dirname, join as pathjoin

DEFAULT_OSX_SYSTEM_PYTHON = '/usr/bin/python'


def load_config():
    config = pathjoin(dirname(__file__), "buildconfig.ini")
    cp = ConfigParser.ConfigParser()
    if not os.path.exists(config):
        with open(config, 'w') as f:
            cp.add_section("General")
            cp.set("General", "pypy", pathjoin(dirname(__file__), "pypy"))
            cp.set("General", "rsdl", pathjoin(dirname(__file__), "rsdl"))
            cp.set("General", "sqpyte", pathjoin(dirname(__file__), "sqpyte"))
            cp.set("General", "topaz", pathjoin(dirname(__file__), "topaz"))
            cp.set("General", "rply", pathjoin(dirname(__file__), "rply"))
            cp.set("General", "appdirs", pathjoin(dirname(__file__), "appdirs"))
            cp.set("General", "squeak", pathjoin(dirname(__file__), "squeak"))
            cp.add_section("Linux")
            cp.set("Linux", "Python32Bit", pathjoin(dirname(__file__), "pypy-linux32", "bin", "pypy"))
            cp.set("Linux", "pygame_cffi", pathjoin(dirname(__file__), "pygame_cffi"))
            cp.set("Linux", "SDL32bit", pathjoin(dirname(__file__), "SDL32bit"))
            cp.add_section("Windows")
            cp.set("Windows", "Python32Bit", pathjoin(dirname(__file__), "pypy-win32", "pypy.exe"))
            cp.set("Windows", "SDL", pathjoin(dirname(__file__), "SDL"))
            cp.set("Windows", "WindowsSDK7", "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A")
            cp.set("Windows", "VisualStudio9", "C:\\Program Files (x86)\\Microsoft Visual Studio 9.0")
            cp.set("Windows", "Graphviz", pathjoin(dirname(__file__), "Graphviz"))
            cp.set("Windows", "pypyextlibs", pathjoin(dirname(__file__), "pypyextlibs"))
            cp.write(f)
        print "I've just added a config file at %s. Please review the values and run `install_dependencies` (if needed)." % config
    else:
        cp.read(config)
    return cp, config


def ensure_32bit_environment_if_required():
    if "64bit" in platform.architecture()[0] and "download_dependencies" not in sys.argv[0] and "--32bit" in sys.argv:
        sys.argv.remove("--32bit")
        import signal
        if os.name == "nt":
            py = cp.get("Windows", "Python32Bit")
            print "Have to switch to 32-bit Python in %s. Visual Studio breakpoints will not work!!" % py
            print "To make them work, setup the 32-bit Python in your Visual Studio environment"
            child = subprocess.Popen([py] + sys.argv)
        elif "linux" in sys.platform:
            py = cp.get("Linux", "Python32Bit")
            print "Have to switch to 32-bit Python in %s." % py
            print "If this doesn't work, make sure you have the following 32-bit packages installed (Ubuntu 14.10 names):"
            print "\tlibffi6:i386 libffi-dev:i386 libffi-dev libc6:i386 libc6-dev-i386 libbz2-1.0:i386 libexpat1:i386 zlib1g:i386 libssl1.0.0:i386 libgcrypt11:i386 libtinfo5:i386 libsdl1.2-dev:i386 gcc-multilib"
            print "If you cannot install these, consider setting up a 32-bit chroot."
            os.environ["CC"] = os.getenv("CC", "cc") + " -m32"
            os.environ["CFLAGS"] = os.getenv("CFLAGS", "") + " -m32"
            os.environ["PYTHONPATH"] = os.getenv("PYTHONPATH", "") + ":" + cp.get("Linux", "pygame_cffi")
            if cp.has_option("Linux", "SDL32bit"):
                os.environ["SDL_PREFIX"] = cp.get("Linux", "SDL32bit")
            child = subprocess.Popen([py] + sys.argv)
        elif "darwin" == sys.platform and "64bit" in platform.architecture()[0]:
            if not os.environ.get("VERSIONER_PYTHON_PREFER_32_BIT"):
                print "Trying to switch to 32-bit Python by setting $VERSIONER_PYTHON_PREFER_32_BIT=true."
                os.environ["VERSIONER_PYTHON_PREFER_32_BIT"] = "yes"
                child = subprocess.Popen([sys.executable] + sys.argv)
            else:
                if os.path.isfile(DEFAULT_OSX_SYSTEM_PYTHON):
                    print "Trying to use system Python."
                    child = subprocess.Popen([DEFAULT_OSX_SYSTEM_PYTHON] + sys.argv)
                else:
                    print "Could not locate and use system Python."
                    sys.exit(1)
        else:
            raise AssertionError("Unsupported platform")

        def handler(signum, frame):
            child.send_signal(signum)
        for sig in ["SIGTERM", "SIGINT", "SIGKILL"]:
            try:
                signal.signal(getattr(signal, sig), handler)
            except Exception:
                pass
        exit(child.wait())


def prepare_environment_variables():
    if "nt" == os.name:
        section = "Windows"
        vs = cp.get("Windows", "VisualStudio9")
        sdk = cp.get("Windows", "WindowsSDK7")
        pypyextlibs = cp.get("Windows", "pypyextlibs")
        paths = [pathjoin(vs, "VC"), pathjoin(vs, "VC", "atlmfc"), pypyextlibs, sdk]
        os.environ['INCLUDE'] = ";".join([pathjoin(e, "include") for e in paths])
        os.environ['LIB'] = ";".join([pathjoin(e, "lib") for e in paths])
        os.environ['LIBPATH'] = ";".join([pathjoin(e, "lib") for e in paths[0:3]])
        os.environ['Path'] = ";".join([pathjoin(vs, "VC", "bin"),
                                       pathjoin(vs, "Common7", "IDE"),
                                       pathjoin(sdk, "Bin"),
                                       pathjoin(cp.get("Windows", "Graphviz"), "bin"),
                                       pathjoin(pypyextlibs, "bin"),
                                       os.environ["Path"]])
        os.environ["SDL_PREFIX"] = cp.get("Windows", "SDL")
    elif "linux" in sys.platform:
        section = "Linux"
    elif "darwin" == sys.platform:
        section = "macOS"
        try:
            # Check if sdl2-config is installed (e.g. when SDL2 is installed via brew)
            os.environ["SDL_PREFIX"] = subprocess.check_output(['sdl2-config', '--prefix']).strip()
        except:
            pass
    else:
        raise AssertionError("Unsupported platform")
    for dependency in ["pypy", "rsdl", "sqpyte", "topaz", "rply", "appdirs"]:
        try:
            sys.path.insert(0, cp.get(section, dependency))
        except (ConfigParser.NoOptionError, ConfigParser.NoSectionError):
            sys.path.insert(0, cp.get("General", dependency))
    try:
        import targetrsqueak as rsqueak
    except ImportError:
        sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))


def fix_nt():
    if "nt" == os.name:
        import codecs
        codecs.register(lambda name: codecs.lookup('utf-8') if name == 'cp65001' else None)


cp, config = load_config()
if cp.has_option("Linux", "SDL64bit"):
    os.environ["SDL_PREFIX"] = cp.get("Linux", "SDL64bit")
if "--64bit" in sys.argv:
    sys.argv.remove("--64bit")
ensure_32bit_environment_if_required()
prepare_environment_variables()
fix_nt()
