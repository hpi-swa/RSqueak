#! /usr/bin/env python

#! /usr/bin/env python

import sys, zlib, base64
from os import path
from environment import cp

if __name__ == "__main__":
    if not reduce(bool.__or__, [arg.find("/test") != -1 or arg.find("\\test") != -1
                                for arg in sys.argv]):
        sys.argv.append(path.join(path.dirname(__file__), "..", "spyvm", "test"))

    import pytest
    exit(pytest.main(args=sys.argv))
