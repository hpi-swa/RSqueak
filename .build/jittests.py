#! /usr/bin/env python

import sys
from os import path
from environment import cp, config

if __name__ == "__main__":
    if not any(arg.startswith("--jit") for arg in sys.argv):
        sys.argv.append("--jit=%s" % path.join(path.dirname(__file__), "..", "rsqueak"))
    sys.argv.append("-s")
    sys.argv.append("-vv")
    if not any(arg.find("jittest/test") != -1 for arg in sys.argv):
        sys.argv.append(path.join(path.dirname(__file__), "..", "rsqueakvm", "test", "jittest"))

    import pytest
    exit(pytest.main(args=sys.argv[1:]))
