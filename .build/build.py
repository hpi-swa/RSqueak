#! /usr/bin/env python

import sys, os


if __name__ == "__main__":
    if not any(arg.startswith("-") for arg in sys.argv):
        sys.argv.append("--batch")
    target = os.path.join(os.path.dirname(__file__), "..", "targetrsqueak.py")
    if '--' in sys.argv:
      sys.argv[sys.argv.index('--')] = target
    else:
      sys.argv.append(target)
    import environment
    from rpython.translator.goal.translate import main
    main()
