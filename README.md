## RSqueak/VM

A Squeak VM written in RPython.

Prebuilt binaries
* [Linux](http://www.lively-kernel.org/babelsberg/RSqueak/rsqueak-linux-latest) [![Linux Build Status](https://travis-ci.org/HPI-SWA-Lab/RSqueak.png?branch=master)](https://travis-ci.org/HPI-SWA-Lab/RSqueak)
* [Mac OS X](http://www.lively-kernel.org/babelsberg/RSqueak/rsqueak-darwin-latest) [![Max OS X Build Status](https://travis-ci.org/timfel/RSqueak-MacOSXBuild.png?branch=master)](https://travis-ci.org/timfel/RSqueak-MacOSXBuild)
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

###### Mac OS X

RSqueak/VM currently needs to be compile using a 32-bit python and
32-bit clang. To do so, run

    export VERSIONER_PYTHON_PREFER_32_BIT=yes

before you run any of the python scripts in the `.build`
directory. You also need to download SDL-1.2 as a framework. Check the
`.travis/build-osx.sh` if you get stuck anywhere.
