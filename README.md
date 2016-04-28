## RSqueak/VM [![Linux Build Status](https://travis-ci.org/HPI-SWA-Lab/RSqueak.svg?branch=master)](https://travis-ci.org/HPI-SWA-Lab/RSqueak) [![Windows Build Status](https://ci.appveyor.com/api/projects/status/e37a79tt5irr7sx1/branch/master?svg=true)](https://ci.appveyor.com/project/timfel/rsqueak) [![Coverage Status](https://coveralls.io/repos/github/HPI-SWA-Lab/RSqueak/badge.svg?branch=master)](https://coveralls.io/github/HPI-SWA-Lab/RSqueak?branch=master)

A Squeak VM written in RPython.

### Download

All-in-One bundle for Linux, Windows and OS X:

[![Download zip](https://img.shields.io/badge/Download-zip-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/bundle/RSqueak.zip) [![Download tar.gz](https://img.shields.io/badge/Download-tar.gz-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/bundle/RSqueak.tar.gz)

Pre-built binaries:

[![Download Linux](https://img.shields.io/badge/Download-Linux-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-latest) [![Download Mac OS X](https://img.shields.io/badge/Download-Mac_OS_X-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-darwin-latest) [![Download Windows](https://img.shields.io/badge/Download-Windows-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-win32-latest.exe)

We also have experimental builds for Raspberry Pi:

[![Download Raspberry Pi 1](https://img.shields.io/badge/Download-Raspberry_Pi_1-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-armv6raspbian-latest) [![Download Raspberry Pi 2](https://img.shields.io/badge/Download-Raspberry_Pi_2-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-armv7-araspbian-latest) [![Download Raspberry Pi 3](https://img.shields.io/badge/Download-Raspberry_Pi_3-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-armv8-araspbian-latest)

We do build 64-bit virtual machines, but these are not fully functional,
yet. They can be used to open both 32-bit and 64-bit images, but bugs remain
that can cause slowdowns or crashes. We are actively investigating 64-bit
support and some benchmarks show promising results. Due to limitations of the
underlying RPython toolchain, Windows binaries cannot currently be built in
64-bit mode.

[![Download Linux x86_64](https://img.shields.io/badge/Download-Linux_x86__64-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-x86_64-latest) [![Download Mac OS X x86_64](https://img.shields.io/badge/Download-Mac%20OS%20X%20x86__64-blue.svg)](https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-darwin-x86_64-latest)

### Finding a working image

Although RSqueak can load images starting with Squeak 2, many primitives are not
implemented and instead rely on in-image fallback code to be available and
correct. This is why only a Trunk image of Squeak with the latest version of
VMMaker from the VMMaker.oscog branch fully works. Try this in a recent Trunk
image to prepare it for use with RSqueak/VM:

```Smalltalk
(Installer squeak project: 'VMMaker') install: 'VMMaker.oscog'.
MCMcmUpdater updateFromServer.
```

### Building from Source

###### Common to all systems

We have scripts for installing dependencies, building, running the
unit tests, and running JIT tests in the `.build` subdirectory. You
need the `PyPy` source tree, and the `rsdl` source tree. If you
already have all these, run `.build/build.py`. It will generate a
config file `.build/buildconfig.ini` where you can set your paths. You
can also run `.build/download_dependencies.py` to download the
dependencies automatically. You will also need a Python and a C
compiler for 32-bit compilation.

###### Windows

On Windows, you will have to use the C compiler that comes with Visual
Studio 2008, because newer ones crash the JIT. It suffices to just
install the [Microsoft C compiler V90 plus Windows SDK
7](https://github.com/HPI-SWA-Lab/RSqueak/releases/download/Dependencies/vc_stdx86.zip). The
packages provided in the link install it to the default paths. If you
already have the compiler and SDK, you can also just update the paths
in `.build/buildconfig.ini`.

###### Linux

RSqueak/VM currently needs to be compiled using a 32-bit python and SDL2 using
32-bit libraries for everything. The easiest way to ensure that is to use a
chroot, but you can also install the `:i386` versions of the SDL2 dependencies
for your distro.

###### Mac OS X

RSqueak/VM currently needs to be compiled using a 32-bit python and
32-bit clang. To do so, run

```bash
export VERSIONER_PYTHON_PREFER_32_BIT=yes
```

before you run any of the python scripts in the `.build` directory. You also
need to download SDL2 as a framework (homebrew version is not tested). Check
the `.travis/build-osx.sh` if you get stuck anywhere.

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
`rsqueakvm/test` directory (but not those in `rsqueakvm/test/jittest/`). This
is a standalone pytest script, so you can pass arguments or select single
test files as you would for pytest.

###### jittests.py

This script requires that you have already built an `rsqueak` binary and
that you have the C Squeak VM installed. It executes the tests in
`rsqueakvm/test/jittest/` and checks for the JIT output. We use these tests to
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

###### shell.py

This script sets all the environment variables as if for translating the VM,
and the drops you into an interactive Python prompt. You can type `terminal()`
to drop into a system shell (`%COMSPEC%` on Windows `$SHELL` or `/bin/sh` on
Unices) which then has all the environment variables set up that you need to
do manual partial translations or similar things.
