import sys

from topaz.main import get_topaz_config_options
from topaz.objspace import ObjectSpace

from rpython.config.translationoption import get_combined_translation_config


def new_topaz_objspace():
    translating = sys.argv[0] == '.build/build.py'  # make better
    config = get_combined_translation_config(translating=translating)
    config.set(**get_topaz_config_options())
    config.translation.suggest(check_str_without_nul=True)
    config.objspace.usemodules._vmprof = False
    return ObjectSpace(config)

ruby_space = new_topaz_objspace()
