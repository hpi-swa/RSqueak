#! /usr/bin/env python
import sys
import os

from rsqueakvm.util import system

from rpython.config.config import to_optparse
from rpython.config.translationoption import get_combined_translation_config
from rpython.jit.codewriter.policy import JitPolicy
from rpython.rlib import objectmodel


sys.setrecursionlimit(15000)


def target(driver, args):
    driver.exe_name = "rsqueak"
    config = driver.config
    parser(config).parse_args(args)

    driver.config.translation.suggest(**{
        "jit": True,
        # "jit_opencoder_model": "big", # this is only needed if we want to have huge trace length (> TRACE_LIMIT)
    })
    # driver.config.translation.set(gcrootfinder="shadowstack") # let's stick with automatic
    if system.IS_WINDOWS:
        driver.config.translation.suggest(**{
            "icon": os.path.join(os.path.dirname(__file__), "rsqueak.ico")
        })
    config.translating = True

    system.expose_options(driver.config)
    # We must not import this before the config was exposed
    from rsqueakvm.main import safe_entry_point
    return safe_entry_point, None

def jitpolicy(self):
    return JitPolicy()

def parser(config):
    return to_optparse(config, useoptions=["rsqueak.*"])

def print_help(config):
    to_optparse(config).print_help()

take_options = True

def get_additional_config_options():
    return system.translation_options()


if __name__ == '__main__':
    assert not objectmodel.we_are_translated()
    from rpython.translator.driver import TranslationDriver
    driver = TranslationDriver()
    driver.config = get_combined_translation_config(
        system.translation_options(),
        translating=False)
    idx = sys.argv.index("--")
    if idx >= 0:
        configargs, args = sys.argv[0:idx], sys.argv[idx:]
    else:
        configargs, args = [], sys.argv
    f, _ = target(driver, configargs)
    try:
        sys.exit(f(args))
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
