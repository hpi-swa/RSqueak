import sys
import os
import platform

from rpython.config.config import OptionDescription, Option, BoolOption, \
    IntOption, StrOption, ArbitraryOption, FloatOption, DEFAULT_OPTION_NAME
from rpython.config.translationoption import get_combined_translation_config


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

def translation_options():
    """NOT RPYTHON"""
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
                default="", cmdline="--disable_plugins"
            ),
            BoolOption(
                "without_plugins",
                "Disable all plugins",
                default=False, cmdline="--without_plugins"
            ),
        ]
    )

def expose_options(config):
    """NOT RPYTHON"""
    for name in translation_options().getpaths():
        globals()[name] = getattr(config.rsqueak, name)
    globals()["translationconfig"] = config.translation

# Expose an empty default
expose_options(get_combined_translation_config(translation_options(), translating=False))
