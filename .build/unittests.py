#! /usr/bin/env python

#! /usr/bin/env python

import sys
from os import path, walk
import environment

# These should run alone, because they call allInstances and become, which is
# hard on memory and can break on some PyPy releases if run late in the game
SHOULD_RUN_FIRST = [
    "test_miniimage.py",
    "test_primitives.py",
    "test_miniimage_compiling.py"
]

if __name__ == "__main__":
    import pytest
    if not any("/test" in arg or "\\test" in arg for arg in sys.argv):
        testdir = path.join(path.dirname(__file__), "..", "rsqueakvm", "test")
        exitcode = 0
        args = sys.argv[:]
        for n in SHOULD_RUN_FIRST:
            args.append(path.join(testdir, n))
            exitcode |= pytest.main(args=args)
            args.pop()
        args = sys.argv[:]
        for root, subdirs, files in walk(testdir):
            files.sort()
            for f in files:
                if f.startswith("test_") and f.endswith(".py") and (not f in SHOULD_RUN_FIRST):
                    args.append(path.join(root, f))
        exitcode |= pytest.main(args=args)
        exit(exitcode)
    else:
        exit(pytest.main(args=sys.argv))
