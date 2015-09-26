#! /usr/bin/env python

#! /usr/bin/env python

import sys
from os import path
import environment

if __name__ == "__main__":
    if not any("/test" in arg or "\\test" in arg for arg in sys.argv):
        sys.argv.append(path.join(path.dirname(__file__), "..", "spyvm", "test"))

    import pytest
    exit(pytest.main(args=sys.argv))
