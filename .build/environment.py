#! /usr/bin/env python

import sys, os, platform, subprocess, ConfigParser
from os.path import dirname, join as pathjoin


def load_config():
    config = pathjoin(dirname(__file__), "buildconfig.ini")
    cp = ConfigParser.ConfigParser()
    if not os.path.exists(config):
        with open(config, 'w') as f:
            cp.add_section("General")
            cp.set("General", "pypy", pathjoin(dirname(__file__), "pypy"))
            cp.set("General", "rsdl", pathjoin(dirname(__file__), "rsdl"))
            cp.set("General", "squeak", pathjoin(dirname(__file__), "squeak"))
            cp.add_section("Linux")
            cp.set("Linux", "Python32Bit", pathjoin(dirname(__file__), "pypy-linux32", "bin", "pypy"))
            cp.set("Linux", "pygame_cffi", pathjoin(dirname(__file__), "pygame_cffi"))
            cp.add_section("Windows")
            cp.set("Windows", "Python32Bit", pathjoin(dirname(__file__), "pypy-win32", "pypy.exe"))
            cp.set("Windows", "SDL", pathjoin(dirname(__file__), "SDL"))
            cp.set("Windows", "WindowsSDK7", "C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v7.1A")
            cp.set("Windows", "VisualStudio9", "C:\\Program Files (x86)\\Microsoft Visual Studio 9.0")
            cp.set("Windows", "Graphviz", pathjoin(dirname(__file__), "Graphviz"))
            cp.write(f)
        print "I've just added a config file at %s. Please review the values and run `install_dependencies` (if needed)." % config
    else:
        cp.read(config)
    return cp, config


def ensure_32bit_environment():
    if "64bit" in platform.architecture()[0] and "download_dependencies" not in sys.argv[0]:
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
            child = subprocess.Popen([py] + sys.argv)
        elif "darwin" == sys.platform and "64bit" in platform.architecture()[0]:
            print "Trying to switch to 32-bit Python by setting VERSIONER_PYTHON_PREFER_32_BIT. You have to run witht the system Python for this to work."
            os.environ["VERSIONER_PYTHON_PREFER_32_BIT"] = "yes"
            child = subprocess.Popen([sys.executable] + sys.argv, shell=True)
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
    sys.path.insert(0, cp.get("General", "pypy"))
    sys.path.insert(0, cp.get("General", "rsdl"))
    try:
        import targetrsqueak as rsqueak
    except ImportError:
        sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
    if "nt" == os.name:
        vs = cp.get("Windows", "VisualStudio9")
        sdk = cp.get("Windows", "WindowsSDK7")
        paths = [pathjoin(vs, "VC"), pathjoin(vs, "VC", "atlmfc"), sdk]
        os.environ['INCLUDE'] = ";".join([pathjoin(e, "include") for e in paths])
        os.environ['LIB'] = ";".join([pathjoin(e, "lib") for e in paths])
        os.environ['LIBPATH'] = ";".join([pathjoin(e, "lib") for e in paths[0:2]])
        os.environ['Path'] = ";".join([pathjoin(vs, "VC", "bin"),
                                       pathjoin(vs, "Common7", "IDE"),
                                       pathjoin(sdk, "Bin"),
                                       pathjoin(cp.get("Windows", "Graphviz"), "bin"),
                                       os.environ["Path"]])
        os.environ["SDL_PREFIX"] = cp.get("Windows", "SDL")
    elif "linux" in sys.platform:
        pass
    elif "darwin" == sys.platform:
        pass
    else:
        raise AssertionError("Unsupported platform")


cp, config = load_config()
ensure_32bit_environment()
prepare_environment_variables()
