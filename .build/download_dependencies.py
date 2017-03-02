#! /usr/bin/env python

import os, sys, platform
from environment import cp

def download_and_extract(url, targetdir, callback=None):
    oldcwd = os.getcwd()
    os.chdir(os.path.dirname(__file__))
    try:
        print "Ensuring %s is there" % os.path.abspath(targetdir)
        if os.path.exists(targetdir) and "update" not in sys.argv:
            return
        import urllib
        try:
            filename, headers = urllib.urlretrieve(url)
        except Exception, e:
            print e
            return
        try:
            if os.path.exists(targetdir):
                os.remove(targetdir)
        except Exception, e:
            print e
            return
        if filename.endswith(".zip"):
            import shutil, zipfile
            with zipfile.ZipFile(filename) as f:
                # extractall doesn't work if zipfiles do not include directories
                # as separate members (as for github and bitbucket)
                topdirs = []
                for m in f.namelist():
                    if m.endswith("/"):
                        if len(m.split("/")) == 2:
                            topdirs.append(m.split("/")[0])
                        continue
                    directory = "/".join(m.split("/")[0:-2])
                    if directory.find("/") == -1:
                        topdirs.append(directory)
                    if (len(directory) > 0 and
                        not directory.startswith("/") and
                        (directory.find("..") == -1) and
                        not os.path.exists(directory)):
                        os.makedirs(directory)
                    sys.stdout.write(".")
                    f.extract(m)
                topdirs = filter(lambda x: len(x) != 0, topdirs)
                topdirs = list(set(topdirs))
                if not topdirs:
                    topdirs = [f.namelist()[0]]
                if len(topdirs) > 1:
                    os.makedirs(targetdir)
                    for d in topdirs:
                        shutil.move(d, "%s/%s" % (targetdir, d))
                else:
                    shutil.move(topdirs[0], targetdir)
        elif filename.endswith(".bz2"):
            import tarfile, shutil
            with tarfile.open(filename) as tar:
                tar.extractall()
                topdir = tar.getnames()[0]
                print topdir
                shutil.move(topdir, targetdir)
        elif filename.endswith(".gz"):
            import tarfile, shutil
            with tarfile.open(filename) as tar:
                tar.extractall()
                topdir = tar.getnames()[0]
                print topdir
                shutil.move(topdir, targetdir)
        else:
            raise AssertionError("Unsupported Archive %s" % filename)
        if callback:
            callback(targetdir)
    finally:
        os.chdir(oldcwd)


DEPS = [("https://bitbucket.org/pypy/pypy/get/default.zip", cp.get("General", "pypy")),
        ("https://bitbucket.org/pypy/rsdl/get/sdl2.zip", cp.get("General", "rsdl"))]

if ('DatabasePlugin' in ' '.join(sys.argv) or
    'DatabasePlugin' in os.environ.get('PLUGINS', '')):
    DEPS.append(("https://github.com/HPI-SWA-Lab/SQPyte/archive/rsqueak.zip",
                cp.get("General", "sqpyte")))

if ('RubyPlugin' in ' '.join(sys.argv) or
    'RubyPlugin' in os.environ.get('PLUGINS', '')):
    DEPS.append(("https://github.com/alex/rply/archive/master.zip",
                cp.get("General", "rply")))
    DEPS.append(("https://github.com/ActiveState/appdirs/archive/master.zip",
                cp.get("General", "appdirs")))
    DEPS.append(("https://github.com/topazproject/topaz/archive/tim/fix-translation.zip",
                cp.get("General", "topaz")))


def build_pypy32(exe, directory):
    oldcwd = os.getcwd()
    try:
        os.chdir(directory)
        import urllib
        filename, headers = urllib.urlretrieve("https://bootstrap.pypa.io/get-pip.py")
        os.system("%s %s" % (exe, filename))
        os.system("%s -m pip install pytest-cov" % exe)
    finally:
        os.chdir(oldcwd)

if os.name == "nt":
    DEPS.append(("http://libsdl.org/release/SDL2-devel-2.0.5-VC.zip", cp.get("Windows", "SDL")))
    DEPS.append(("https://bitbucket.org/pypy/pypy/downloads/local_2.4.zip", cp.get("Windows", "pypyextlibs")))
    if os.getenv("APPVEYOR") is None:
        # Don't download this on appveyor, saves time and memory
        DEPS.extend([
            ("https://bitbucket.org/pypy/pypy/downloads/pypy-4.0.1-win32.zip", ("pypy-win32", lambda d: build_pypy32("pypy.exe", d))),
            ("http://www.graphviz.org/pub/graphviz/stable/windows/graphviz-2.38.zip", cp.get("Windows", "Graphviz"))
        ])
elif "64bit" in platform.architecture()[0] and "linux" in sys.platform:
    DEPS.extend([
        ("https://bitbucket.org/pypy/pypy/downloads/pypy-4.0.1-linux.tar.bz2", ("pypy-linux32", lambda d: build_pypy32("bin/pypy", d))),
        ("https://github.com/CTPUG/pygame_cffi/archive/master.zip", "pygame_cffi"),
    ])
else:
    pass


def main():
    for url, dir_or_tuple in DEPS:
        if type(dir_or_tuple) is tuple:
            download_and_extract(url, dir_or_tuple[0], callback=dir_or_tuple[1])
        else:
            download_and_extract(url, dir_or_tuple)

if __name__ == '__main__':
    main()
