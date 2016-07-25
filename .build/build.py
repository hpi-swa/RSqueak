#! /usr/bin/env python

import sys, os


"""
RSqueak Build Options (separated with `--` from RPython options)

Example:
  .build/build.py -Ojit -- --64bit

  --64bit                               - Compile for 64bit platform
  --plugins MyPlugin[,AnotherPlugin]    - Comma-separated list of optional plugins
                                          (e.g. DatabasePlugin)
"""

if __name__ == "__main__":
    sys.argv[0] = 'rpython'  # required for sqpyte hacks
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
