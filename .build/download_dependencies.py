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
        ("https://bitbucket.org/pypy/rsdl/get/sdl2.zip", cp.get("General", "rsdl")),
        ("https://github.com/HPI-SWA-Lab/SQPyte/archive/rsqueak.zip", cp.get("General", "sqpyte"))]

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
    DEPS.append(("http://libsdl.org/release/SDL2-devel-2.0.3-VC.zip", cp.get("Windows", "SDL")))
    if os.getenv("APPVEYOR") is None:
        # Don't download this on appveyor, saves time and memory
        DEPS.extend([
            ("https://bitbucket.org/pypy/pypy/downloads/pypy-4.0.1-win32.zip", ("pypy-win32", lambda d: build_pypy32("pypy.exe", d))),
            ("http://www.graphviz.org/pub/graphviz/stable/windows/graphviz-2.38.zip", cp.get("Windows", "Graphviz"))
        ])
elif "64bit" in platform.architecture()[0] and "linux" in sys.platform:
    def build_sdl2(directory):
        print "Building 32-bit SDL2 locally, this is only needed once and only on 64-bit Linux"
        print "A log of the build can be found in %s" % os.path.abspath(os.path.join(directory, "..", "sdl2build.log"))
        oldcwd = os.getcwd()
        for f in ["CFLAGS", "CXXFLAGS", "LDFLAGS"]:
            locals()["old" + f] = os.getenv(f, None)
            os.environ[f] = "-m32"
        try:
            os.chdir(directory)
            prefix = os.path.abspath(cp.get("Linux", "SDL32bit"))
            retval = os.system("./configure --prefix=%s --build=i686-pc-linux-gnu >../sdl2build.log 2>&1" % prefix)
            retval |= os.system("make >> ../sdl2build.log 2>&1")
            retval |= os.system("make install >> ../sdl2build.log 2>&1")
            if retval != 0:
                print "\nThere was an error building 32-bit SDL2. RSqueak will not work without! See the .build/sdl2build.log file for more info."
                print "You might be able to just install 32-bit packages or install it manually."
                print "If so, just set the SDL32bit variable in the buildconfig.ini to point to the right place, or remove it, if SDL is installed globally"
        finally:
            for f in ["CFLAGS", "CXXFLAGS", "LDFLAGS"]:
                if locals()["old" + f]:
                    os.environ[f] = locals()["old" + f]
            os.chdir(oldcwd)

    DEPS.extend([
        ("https://bitbucket.org/pypy/pypy/downloads/pypy-4.0.1-linux.tar.bz2", ("pypy-linux32", lambda d: build_pypy32("bin/pypy", d))),
        ("https://github.com/CTPUG/pygame_cffi/archive/master.zip", "pygame_cffi"),
        ("https://www.libsdl.org/release/SDL2-2.0.3.tar.gz", ("SDL2-src", build_sdl2))
    ])
else:
    pass


if __name__ == '__main__':
    for url, dir_or_tuple in DEPS:
        if type(dir_or_tuple) is tuple:
            download_and_extract(url, dir_or_tuple[0], callback=dir_or_tuple[1])
        else:
            download_and_extract(url, dir_or_tuple)
