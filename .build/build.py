#! /usr/bin/env python

import sys, os

if __name__ == "__main__":
    if not reduce(bool.__or__, [arg.startswith("-O") for arg in sys.argv]):
        sys.argv.append("--batch")
        sys.argv.append("-Ojit")
    sys.argv.append(os.path.join(os.path.dirname(__file__), "..", "targetrsqueak.py"))
    import environment
    from rpython.translator.goal.translate import main
    main()
