#! /usr/bin/env python

import sys
from os import path
from environment import cp, config  # import with side effects

from rsqueakvm.util import system

if __name__ == "__main__":
    try:
        plugins = next(arg for arg in sys.argv
                       if arg.startswith("--plugins="))
        sys.argv.remove(plugins)
    except StopIteration:
        pass
    try:
        plugin_dir = next(arg for arg in sys.argv
                          if arg.startswith("--plugin-dir="))
        sys.argv.remove(plugin_dir)
    except StopIteration:
        print "No plugin directory provided via --plugin-dir=<plugin directory>"
        sys.exit(1)
    if plugins:
        plugins = plugins.split("=")[1]
        system.optional_plugins = plugins
    plugin_dir = plugin_dir.split("=")[1]
    sys.argv.append("-s")
    sys.argv.append("-vv")
    sys.argv.append(path.join(
        path.dirname(__file__), "..",
        "rsqueakvm", "test", "plugins", plugin_dir))

    import pytest
    exit(pytest.main(args=sys.argv[1:]))
