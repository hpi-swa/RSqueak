#! /usr/bin/env python

import os, sys, platform
from environment import cp

def download_and_extract(url, targetdir):
    oldcwd = os.getcwd()
    os.chdir(os.path.dirname(__file__))
    try:
        print "Ensuring %s is there" % os.path.abspath(targetdir)
        if os.path.exists(targetdir): return
        import urllib
        filename, headers = urllib.urlretrieve(url)
        if filename.endswith(".zip"):
            import shutil, zipfile
            with zipfile.ZipFile(filename) as f:
                # extractall doesn't work if zipfiles do not include directories
                # as separate members (as for github and bitbucket)
                for m in f.namelist():
                    if m.endswith("/"): continue
                    directory = "/".join(m.split("/")[0:-2])
                    if directory.find("/") == -1: topdir = directory
                    if (len(directory) > 0 and
                        not directory.startswith("/") and
                        (directory.find("..") == -1) and
                        not os.path.exists(directory)):
                        os.makedirs(directory)
                    sys.stdout.write(".")
                    f.extract(m)
                if not topdir:
                    topdir = f.namelist()[0]
                print topdir
                shutil.move(topdir, targetdir)
        elif filename.endswith(".bz2"):
            import tarfile, shutil
            with tarfile.open(filename) as tar:
                tar.extractall()
                topdir = tar.getnames()[0]
                print topdir
                shutil.move(topdir, targetdir)
        else:
            raise AssertionError("Unsupported Archive %s" % filename)
    finally:
        os.chdir(oldcwd)


DEPS = [("https://bitbucket.org/pypy/pypy/get/default.zip", cp.get("General", "pypy")),
            ("https://bitbucket.org/pypy/rsdl/get/default.zip", cp.get("General", "rsdl"))]
if os.name == "nt":
    DEPS.extend([("https://bitbucket.org/pypy/pypy/downloads/pypy-2.5.1-win32.zip", "pypy-win32"),
                    ("http://libsdl.org/release/SDL-devel-1.2.15-VC.zip", cp.get("Windows", "SDL"))])
elif "64bit" in platform.architecture()[0] and "linux" in sys.platform:
    DEPS.extend([("https://bitbucket.org/pypy/pypy/downloads/pypy-2.5.1-linux.tar.bz2", "pypy-linux32")])
else:
    pass


if __name__ == '__main__':
    for url, dir in DEPS:
        download_and_extract(url, dir)
