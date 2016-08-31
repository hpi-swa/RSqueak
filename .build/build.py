#! /usr/bin/env python

import sys, os


if os.name == "nt" and (not os.environ.get("APPVEYOR", None)):
    # To get color on Windows, even when it is piped through a redirect:
    #
    # a) We need to block the colorama import, because only through ctypes does
    # color setting actually work.
    #
    # b) We need to wrap stdout and stderr in a PseudoTTY that returns True for
    # isatty()
    class ImportBlocker(object):
        def __init__(self, *args):
            self.module_names = args
        def find_module(self, fullname, path=None):
            if fullname in self.module_names:
                return self
            return None
        def load_module(self, name):
            raise ImportError("%s is blocked and cannot be imported" % name)
    sys.meta_path = [ImportBlocker('colorama')]

    class PseudoTTY(object):
        def __init__(self, std):
            self.__std = std
        def __getattr__(self, name):
            return getattr(self.__std, name)
        def isatty(self):
            return True
    sys.stdout = PseudoTTY(sys.stdout)
    sys.stderr = PseudoTTY(sys.stderr)


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
