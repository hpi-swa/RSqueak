#! /usr/bin/env python

import sys, os

if __name__ == "__main__":
    import environment, code
    print "\033[33;5;7m \nUse terminal() to drop into a system console\n \033[0m"
    def terminal():
        module_path = os.path.abspath(os.path.dirname(__file__))
        if module_path in sys.path:
            paths = sys.path[:sys.path.index(module_path)]
        else:
            paths = sys.path
        os.putenv("PYTHONPATH", os.pathsep.join(paths))
        os.system(os.environ.get("COMSPEC") or os.environ.get("SHELL") or "/bin/sh")
    code.interact(local=locals())
