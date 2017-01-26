#! /usr/bin/env python

import sys
import subprocess
from os import path, walk
import environment

# These should run alone, because they call allInstances and become or do stuff
# with cffi and the display, which isn't always clean and also hard on memory
# and can break on some PyPy releases if run late in the game
SHOULD_RUN_FIRST = [
    "test_display.py",
    "test_model.py",
    "test_miniimage.py",
    "test_primitives.py",
    "test_miniimage_compiling.py",
]

if __name__ == "__main__":
    import pytest

    if not any("/test" in arg or "\\test" in arg for arg in sys.argv):
        testdir = path.join(path.dirname(__file__), "..", "rsqueakvm", "test")
        exitcode = 0
        args = sys.argv[:]
        for n in SHOULD_RUN_FIRST:
            args.append(path.join(testdir, n))
            exitcode |= subprocess.Popen("%s \"%s\"" % (sys.executable, "\" \"".join(args)), shell=True).wait()
            args.pop()
        args = sys.argv[:]
        for root, subdirs, files in walk(testdir):
            files.sort()
            for f in files:
                if (f.startswith("test_") and f.endswith(".py") and
                        (f not in SHOULD_RUN_FIRST)):
                    args.append(path.join(root, f))
        exitcode |= subprocess.Popen("%s \"%s\"" % (sys.executable, "\" \"".join(args)), shell=True).wait()
        exit(exitcode)
    else:
        exit(pytest.main(args=sys.argv[1:]))
