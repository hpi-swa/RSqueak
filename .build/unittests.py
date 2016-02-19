#! /usr/bin/env python

import sys, os
import environment

if __name__ == "__main__":
    if not any("/test" in arg or "\\test" in arg for arg in sys.argv):
        sys.argv.append(os.path.join(os.path.dirname(__file__), "..", "spyvm", "test"))
    os.environ["SDL_VIDEODRIVER"] = "dummy"
    import pytest
    exit(pytest.main(args=sys.argv))
