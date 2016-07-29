import sys
import os
import platform

from rpython.config.config import OptionDescription, Option, BoolOption, \
    IntOption, ArbitraryOption, FloatOption, DEFAULT_OPTION_NAME
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


class ListOption(Option):
    opt_type = "string"

    def __init__(self, name, doc, default=[], cmdline=DEFAULT_OPTION_NAME):
        super(ListOption, self).__init__(name, doc, cmdline)
        self.default = default

    def validate(self, value):
        return True

    def setoption(self, config, value, who):
        return super(ListOption, self).setoption(config, value.split(","), who)


def translation_options():
    """NOT RPYTHON"""
    return OptionDescription(
        "rsqueak", "RSqueak Options", [
            ListOption(
                "optional_plugins",
                "Which optional plugins should be enabled (a comma-separated "\
                "list, e.g. 'ruby_plugin,database_plugin')",
                default=[], cmdline="--plugins"
            )
        ]
    )

def expose_options(config):
    """NOT RPYTHON"""
    for name in translation_options().getpaths():
        globals()[name] = getattr(config.rsqueak, name)
    globals()["translationconfig"] = config.translation

# Expose an empty default
expose_options(get_combined_translation_config(translation_options(), translating=False))
