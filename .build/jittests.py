#! /usr/bin/env python

import sys, zlib, base64
from os import path
from environment import cp

if __name__ == "__main__":
    if not reduce(bool.__or__, [arg.startswith("--jit") for arg in sys.argv]):
        sys.argv.append("--jit=%s" % path.join(path.dirname(__file__), "..", "rsqueak"))
    if not reduce(bool.__or__, [arg.startswith("--squeak") for arg in sys.argv]):
       sys.argv.append("--squeak=%s" % cp.get("General", "squeak"))
    if not reduce(bool.__or__, [arg.find("jittest/test") != -1 for arg in sys.argv]):
        sys.argv.append(path.join(path.dirname(__file__), "..", "spyvm", "test", "jittest"))

    import pytest
    pytest.main(args=sys.argv)
