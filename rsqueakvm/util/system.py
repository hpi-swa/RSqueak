import sys
import os
import platform

from rpython.config.config import OptionDescription, BoolOption, \
    IntOption, StrOption, ArbitraryOption, FloatOption, DEFAULT_OPTION_NAME
from rpython.config.translationoption import get_combined_translation_config
from rpython.rlib.objectmodel import not_rpython

IS_POSIX = os.name == "posix"
IS_WINDOWS = os.name == "nt"
IS_LINUX = "linux" in sys.platform
IS_64BIT = "64bit" in platform.architecture()[0]
IS_CYGWIN = "cygwin" == sys.platform
IS_DARWIN = "darwin" == sys.platform
IS_ARM = "--platform=arm" in sys.argv
IS_SPHINX = "sphinx" in sys.modules
IS_SHELL = '--shell' in sys.argv

if IS_WINDOWS and (not any(arg.startswith("-Ojit") for arg in sys.argv)):
    # XXX: Ugly hack to enable compiling with -O2 on Windows, where
    # platform.uname fails translation (but not with JIT!?!)
    uname = platform.uname()
    def win32uname():
        return uname
    platform.uname = win32uname

@not_rpython
def translation_options():
    return OptionDescription(
        "rsqueak", "RSqueak Options", [
            StrOption(
                "optional_plugins",
                "Which optional plugins should be enabled (a comma-separated "\
                "list, e.g. 'RubyPlugin,DatabasePlugin,JitHooks')",
                default="", cmdline="--plugins"
            ),
            StrOption(
                "disabled_plugins",
                "Which default plugins should be disabled (a comma-separated "\
                "list, e.g. 'LargeIntegers,SocketPlugin,SqueakSSL')",
                default="", cmdline="--disabled_plugins"
            ),
            BoolOption(
                "without_plugins",
                "Disable all plugins",
                default=False, cmdline="--without_plugins"
            ),
        ]
    )

@not_rpython
def expose_options(config):
    for name in translation_options().getpaths():
        globals()[name] = getattr(config.rsqueak, name)
    globals()["translationconfig"] = config.translation

# Expose an empty default
expose_options(get_combined_translation_config(translation_options(), translating=False))
