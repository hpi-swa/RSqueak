#! /usr/bin/env python

import os, sys, platform, urllib, shutil, zipfile, tarfile
from environment import cp


class Chdir(object):
    def __init__(self, path):
        self.old = os.getcwd()
        self.new = path

    def __enter__(self):
        os.chdir(self.new)

    def __exit__(self, *args):
        os.chdir(self.old)


class Dependency(object):
    def __init__(self, url, name, test=None, callback=None):
        self.url = url
        self.path = self.path_from_name(name)
        if type(test) == bool:
            self.test = lambda: test
        elif test is not None:
            self.test = test
        else:
            self.test = lambda: True
        if callback is not None:
            self.callback = callback
        else:
            self.callback = lambda x: x

    def get(self):
        with Chdir(os.path.dirname(__file__)):
            if self.test():
                if self.download_and_extract():
                    self.callback(self.path)

    def path_from_name(self, name):
        if "/" not in name:
            return cp.get("General", name)
        else:
            return cp.get(*name.split("/"))

    def extract_zip(self, filename):
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
                os.makedirs(self.path)
                for d in topdirs:
                    shutil.move(d, "%s/%s" % (self.path, d))
            else:
                shutil.move(topdirs[0], self.path)

    def extract_tar(self, filename):
        with tarfile.open(filename) as tar:
            tar.extractall()
            topdir = tar.getnames()[0]
            print topdir
            shutil.move(topdir, self.path)

    def download_and_extract(self):
        print "Ensuring %s is there for %s" % (os.path.abspath(self.path), self.url)
        if os.path.exists(self.path) and "update" not in sys.argv:
            return False
        try:
            filename, headers = urllib.urlretrieve(self.url)
        except Exception, e:
            print e
            return False
        try:
            if os.path.exists(self.path):
                os.remove(self.path)
        except Exception, e:
            print e
            return False
        if filename.endswith(".zip"):
            self.extract_zip(filename)
        elif filename.endswith(".bz2") or filename.endswith(".gz"):
            self.extract_tar(filename)
        else:
            raise AssertionError("Unsupported Archive %s" % filename)
        return True


def plugin(name):
    return name in (' '.join(sys.argv) + os.environ.get('PLUGINS', ''))

def windows():
    return os.name == "nt"

def local_windows():
    return windows() and not ci()

def linux():
    return "linux" in sys.platform

def linux64():
    return linux() and "64bit" in platform.architecture()[0]

def linux64x32():
    return linux64() and "--32bit" in ' '.join(sys.argv)

def linux32():
    return (linux() and "32bit" in platform.architecture()[0]) or linux64x32()

def ci():
    return os.getenv("TRAVIS", None) or os.getenv("APPVEYOR", None)

def pypywin32(directory):
    buildpypy32("pypy.exe", directory)

def pypylinux32(directory):
    buildpypy32("bin/pypy", directory)

def buildpypy32(exe, directory):
    oldcwd = os.getcwd()
    with Chdir(directory):
        os.chdir(directory)
        import urllib
        filename, headers = urllib.urlretrieve("https://bootstrap.pypa.io/get-pip.py")
        os.system("%s %s" % (exe, filename))
        os.system("%s -m pip install pytest-cov coveralls" % exe)

def buildsdl32(directory):
    oldflags = {}
    for f in ["CFLAGS", "CXXFLAGS", "LDFLAGS"]:
        oldflags[f] = os.getenv(f, None)
        os.environ[f] = "-m32"
    try:
        buildsdl(directory, args="--build=i686-pc-linux-gnu")
    finally:
        for f in ["CFLAGS", "CXXFLAGS", "LDFLAGS"]:
            if oldflags[f]:
                os.environ[f] = oldflags[f]

def buildsdl(directory, args=None):
    args = args or ""
    with Chdir(directory):
        retval = os.system("""
        ./configure --prefix=%s %s >../sdl2build.log 2>&1;
        make -j4 >> ../sdl2build.log 2>&1;
        make install >> ../sdl2build.log 2>&1
        """ % (directory, args))
        if retval != 0:
            print """
            \nThere was an error building SDL2. RSqueak will not work without
            it!  See the .build/sdl2build.log file for more info.  You might be
            able to just install it manually.  If so, just set the SDL32 and
            SDL64 variables in the buildconfig.ini to point to the right place,
            or remove it, if SDL is installed globally
            """

DEPS = [
    Dependency("https://bitbucket.org/pypy/pypy/get/default.zip", "pypy"),
    Dependency("https://bitbucket.org/pypy/rsdl/get/sdl2.zip", "rsdl"),

    Dependency("https://github.com/HPI-SWA-Lab/SQPyte/archive/rsqueak.zip", "sqpyte", test=plugin("DatabasePlugin")),

    Dependency("https://github.com/alex/rply/archive/master.zip", "rply", test=plugin("RubyPlugin")),
    Dependency("https://github.com/ActiveState/appdirs/archive/master.zip", "appdirs", test=plugin("RubyPlugin")),
    Dependency("https://github.com/topazproject/topaz/archive/master.zip", "topaz", test=plugin("RubyPlugin")),

    Dependency("https://bitbucket.org/pypy/pypy/downloads/pypy-4.0.1-win32.zip", "Windows/pypybin", test=windows, callback=pypywin32),
    Dependency("http://libsdl.org/release/SDL2-devel-2.0.5-VC.zip", "Windows/SDL", test=windows),
    Dependency("https://bitbucket.org/pypy/pypy/downloads/local_2.4.zip", "Windows/pypyextlibs", test=windows),
    Dependency("http://www.graphviz.org/pub/graphviz/stable/windows/graphviz-2.38.zip", "Windows/Graphviz", test=local_windows),

    Dependency("https://bitbucket.org/pypy/pypy/downloads/pypy-4.0.1-linux.tar.bz2", "Linux/pypybin", test=linux32, callback=pypylinux32),
    Dependency("https://github.com/CTPUG/pygame_cffi/archive/master.zip", "Linux/pygame_cffi", test=linux32),
    Dependency("http://libsdl.org/release/SDL2-2.0.5.tar.gz", "Linux/SDL32", test=linux32, callback=buildsdl32),
    Dependency("http://libsdl.org/release/SDL2-2.0.5.tar.gz", "Linux/SDL64", test=linux64, callback=buildsdl),
]


def main():
    for dep in DEPS:
        dep.get()


if __name__ == '__main__':
    main()
