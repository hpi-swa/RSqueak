#! /usr/bin/env python

#! /usr/bin/env python

import sys
from os import path
import environment

# These should run first, because they call allInstances and become, which is
# hard on memory and can break on some PyPy releases if run late in the game
SHOULD_RUN_FIRST = ["test_miniimage_compiling.py", "test_primitives.py"]

if __name__ == "__main__":
    import pytest
    if not any("/test" in arg or "\\test" in arg for arg in sys.argv):
        testdir = path.join(path.dirname(__file__), "..", "spyvm", "test")
        for n in SHOULD_RUN_FIRST: sys.argv.append(path.join(testdir, n))
        sys.argv.append(testdir)

    exit(pytest.main(args=sys.argv))
