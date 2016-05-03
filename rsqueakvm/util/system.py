import sys
import os
import platform

IS_POSIX = os.name == "posix"
IS_WINDOWS = os.name == "nt"
IS_LINUX = "linux" in sys.platform
IS_64BIT = "64bit" in platform.architecture()[0]
IS_CYGWIN = "cygwin" == sys.platform
IS_DARWIN = "darwin" == sys.platform
IS_ARM = "--platform=arm" in sys.argv

if IS_WINDOWS and (not any(arg.startswith("-Ojit") for arg in sys.argv)):
    # XXX: Ugly hack to enable compiling with -O2 on Windows, where
    # platform.uname fails translation (but not with JIT!?!)
    uname = platform.uname()
    def win32uname():
        return uname
    platform.uname = win32uname
