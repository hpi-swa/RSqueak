#! /usr/bin/env python

import sys
from os import path
from environment import cp, config  # import with side effects

if __name__ == "__main__":
    try:
        plugin_name = next(arg for arg in sys.argv
                           if arg.startswith("--plugin="))
        sys.argv.remove(plugin_name)
    except StopIteration:
        print "No plugin directory provided via --plugin=<plugin directory>"
        sys.exit(1)
    plugin_name = plugin_name.split("=")[1]
    sys.argv.append("-s")
    sys.argv.append("-vv")
    sys.argv.append(path.join(
        path.dirname(__file__), "..",
        "rsqueakvm", "test", "plugins", plugin_name))

    import pytest
    exit(pytest.main(args=sys.argv[1:]))
