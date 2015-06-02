#! /usr/bin/env python

import sys, os, subprocess

if __name__ == "__main__":
    import environment, code
    print "\033[33;5;7m \nUse terminal() to drop into a system console\n \033[0m"
    def terminal():
        os.system(os.environ.get("COMSPEC") or os.environ.get("SHELL") or "/bin/sh")
    code.interact(local=locals())
