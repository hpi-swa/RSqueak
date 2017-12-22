#! /usr/bin/env python

import sys, os, platform, subprocess, ConfigParser
from os.path import dirname, join as pathjoin

DEFAULT_OSX_SYSTEM_PYTHON = '/usr/bin/python'


def load_config():
    baseconfig = pathjoin(dirname(__file__), "buildconfig.defaults.ini")
    config = pathjoin(dirname(__file__), "buildconfig.ini")
    cp = ConfigParser.SafeConfigParser(
        {"builddir": "%s%s" % (pathjoin(dirname(__file__)), os.path.sep)}
    )
    if not os.path.exists(config):
        print "I've just added a config file at %s. Please review the values and run `install_dependencies` (if needed)." % config
        with open(config, 'w') as f:
            cp.read(baseconfig)
            cp.write(f)
    else:
        cp.read([baseconfig, config])
    return cp, config


def ensure_32bit_environment_if_required():
    if "64bit" in platform.architecture()[0] and "download_dependencies" not in sys.argv[0] and "--32bit" in sys.argv:
        sys.argv.remove("--32bit")
        import signal
        if os.name == "nt":
            py = os.path.join(cp.get("Windows", "pypybin"), "pypy.exe")
            print "Have to switch to 32-bit Python in %s. Visual Studio breakpoints will not work!!" % py
            print "To make them work, setup the 32-bit Python in your Visual Studio environment"
            child = subprocess.Popen([py] + sys.argv)
        elif "linux" in sys.platform:
            py = os.path.join(cp.get("Linux", "pypybin"), "bin/pypy")
            print "Have to switch to 32-bit Python in %s." % py
            print "If this doesn't work, make sure you have the following 32-bit packages installed (Ubuntu 14.10 names):"
            print "\tlibffi6:i386 libffi-dev:i386 libffi-dev libc6:i386 libc6-dev-i386 libbz2-1.0:i386 libexpat1:i386 zlib1g:i386 libssl1.0.0:i386 libgcrypt11:i386 libtinfo5:i386 gcc-multilib"
            print "If you cannot install these, consider setting up a 32-bit chroot."
            os.environ["CC"] = os.getenv("CC", "cc") + " -m32"
            os.environ["CFLAGS"] = os.getenv("CFLAGS", "") + " -m32"
            os.environ["PYTHONPATH"] = os.getenv("PYTHONPATH", "") + ":" + cp.get("Linux", "pygame_cffi")
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
        try:
            mac_ver, _ignore, _ignore2 = platform.mac_ver()
            if len(mac_ver) != 0:
                mac_maj, mac_min, mac_point = mac_ver.split('.')[:3]
                if int(mac_min) >= 11 and not (
                    os.path.isdir('/usr/include/openssl/ssl.h') or
                    os.path.isdir('/usr/local/include/openssl/ssl.h')):
                    #
                    # Since 10.11, OS X no longer ships
                    # openssl system-wide, and Homebrew does not install it system-wide.
                    #
                    # see if header is there:
                    if len(subprocess.check_output(["pkg-config", "openssl", "--cflags-only-I"]).strip()) == 0:
                        if os.path.isdir('/usr/local/opt/openssl/lib/pkgconfig'):
                            if 'PKG_CONFIG_PATH' in os.environ:
                                os.environ['PKG_CONFIG_PATH'] =  '/usr/local/opt/openssl/lib/pkgconfig:' + os.environ['PKG_CONFIG_PATH']
                            else:
                                os.environ['PKG_CONFIG_PATH'] = '/usr/local/opt/openssl/lib/pkgconfig'
                    else:
                        # works nonetheless, ignore
                        pass
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
ensure_32bit_environment_if_required()
if "linux" in sys.platform:
    if "64bit" in platform.architecture()[0]:
        sdlpath = cp.get("Linux", "SDL64", None)
    else:
        sdlpath = cp.get("Linux", "SDL32", None)
    if sdlpath and os.path.exists(os.path.join(sdlpath, "bin/sdl2-config")):
        os.environ["SDL_PREFIX"] = sdlpath
if "--64bit" in sys.argv:
    sys.argv.remove("--64bit")
prepare_environment_variables()
fix_nt()
sys.dont_write_bytecode = True
