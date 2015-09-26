#! /usr/bin/env python

import sys
from os import path
from environment import cp, config

if __name__ == "__main__":
    if not any(arg.startswith("--jit") for arg in sys.argv):
        sys.argv.append("--jit=%s" % path.join(path.dirname(__file__), "..", "rsqueak"))
    if not any(arg.startswith("--squeak") for arg in sys.argv):
        squeak = cp.get("General", "squeak")
        if not path.exists(squeak):
            print "WARN: You need to configure where to find the squeak binary in %s to run the full suite of JIT tests" % config
        sys.argv.append("--squeak=%s" % squeak)
    if not any(arg.find("jittest/test") != -1 for arg in sys.argv):
        sys.argv.append(path.join(path.dirname(__file__), "..", "spyvm", "test", "jittest"))

    import pytest
    exit(pytest.main(args=sys.argv))
