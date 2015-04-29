## RSqueak/VM

A Squeak VM written in RPython.

Prebuilt binaries
* [Linux](http://www.lively-kernel.org/babelsberg/RSqueak/rsqueak-linux-latest) [![Linux Build Status](https://travis-ci.org/HPI-SWA-Lab/RSqueak.svg?branch=master)](https://travis-ci.org/HPI-SWA-Lab/RSqueak)
* [Mac OS X](http://www.lively-kernel.org/babelsberg/RSqueak/rsqueak-darwin-latest) [![Max OS X Build Status](https://travis-ci.org/timfel/RSqueak-MacOSXBuild.svg?branch=master)](https://travis-ci.org/timfel/RSqueak-MacOSXBuild)
* [Windows](http://www.lively-kernel.org/babelsberg/RSqueak/rsqueak-win32-latest.exe) [![Windows Build Status](https://ci.appveyor.com/api/projects/status/e37a79tt5irr7sx1/branch/master?svg=true)](https://ci.appveyor.com/project/timfel/rsqueak)

### Building from Source

###### Common to all systems

We have scripts for installing dependencies, building, running the
unit tests, and running JIT tests in the `.build` subdirectory. You
need the `PyPy` source tree, and the `rsdl` source tree. If you
already have all these, run `.build/build.py`. It will generate a
config file `.build/buildconfig.ini` where you can set your paths. You
can also run `.build/download_dependencies.py` to download the
dependencies automatically. You will also need a python and a C
compiler for 32-bit compilation.

###### Windows

On Windows, you will have to use the C compiler that comes with Visual
Studio 2008, because newer ones crash the JIT. It suffices to just
install the [Microsoft C compiler V90 plus Windows SDK
7](https://dl.dropboxusercontent.com/u/26242153/vc%2B%2B90/vc_stdx86.zip). The
packages provided in the link install it to the default paths. If you
already have the compiler and SDK, you can also just update the paths
in `.build/buildconfig.ini`.

###### Linux

RSqueak/VM currently needs to be compiled using a 32-bit python and
32-bit libraries for everything. The easiest way to ensure that is to
use a chroot (check the file `.travis/setup_chroot.sh` for
pointers). In any case, you'll need to install SDL 1.2. In debian this
can be achieved by running

    apt-get install libsdl1.2-dev

Optionally you can also install FLTK-1.3 if you want to compile a
fallback file chooser when the VM is launched without image argument:

    apt-get install libfltk1.3-dev

###### Mac OS X

RSqueak/VM currently needs to be compiled using a 32-bit python and
32-bit clang. To do so, run

    export VERSIONER_PYTHON_PREFER_32_BIT=yes

before you run any of the python scripts in the `.build`
directory. You also need to download SDL-1.2 as a framework. Check the
`.travis/build-osx.sh` if you get stuck anywhere.

### Developing

The `.build` directory includes several scripts that make development
easier. Once you've setup your system for building, you can use these
to work on the RSqueak/VM.

###### run.py

This script executes RSqueak/VM in *hosted* mode, that is, it runs on
top of a Python interpreter. This is very slow (we recommend using PyPy),
but it can be useful to debug specific aspects of the VM quickly. Ideally,
you set up an image so that it executes the code that you are interested in
early during startup (`DisplayScreen class>>startUp` is a good candidate)
and then you add your breakpoints to the source. You can also pass commandline
arguments to the script or tweak the default arguments in the script itself.

###### unittests.py

The second script that is useful for working on issues regarding the
interpreter is `unittests.py`. By default it runs all tests under the
`spyvm/test` directory (but not those in `spyvm/test/jittest/`). This
is a standalone pytest script, so you can pass arguments or select single
test files as you would for pytest.

###### jittests.py

This script requires that you have already built an `rsqueak` binary and
that you have the C Squeak VM installed. It executes the tests in
`spyvm/test/jittest/` and checks for the JIT output. We use these tests to
ensure that development on the VM does not break JIT optimizations.

###### jit.py

This script is useful to figure out what the JIT is doing. Like `run.py`,
it executes the RSqueak/VM in hosted mode, but this time it also simulates
the JIT. This is even slower, but allows us to test small code snippets
without having to retranslate the entire VM. The code snippets are configured
directly in the file. When you run it, it does part of the translation process
(but only enough to execute your specific code) and then runs the code. When
the JIT kicks in, the compiled loop is shown in a PyGame window. You can then
inspect it at your leisure and when you quit the window, the code continues
executing. In order for this to work, you need to have `pygame` and `graphviz`
installed and in your PATH.

