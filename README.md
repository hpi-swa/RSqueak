RSqueak
=========

A Squeak VM written in RPython.

Setup
----

### Required Projects

You need three repositories: 

* [This one](https://bitbucket.org/pypy/lang-smalltalk)
* [pypy/pypy](https://bitbucket.org/pypy/pypy)
* [pypy/rsdl](https://bitbucket.org/pypy/rsdl)
    * Alternatively download RSDL package from [PYPI](https://pypi.python.org/pypi/rsdl)
    * Then unpack and install it using ```python setup.py install``` or ```pypy setup.py install```

### Required packages

You need the following packages on your OS. Install with your favorite package manager:

* pypy
    * For faster translation of the RSqueak VM. Alternatively use default Python.
* libsdl-dev
* libffi-dev

### Adjusting the PYTHONPATH
In order to allow the RPython toolchain to find the rsdl and pypy packages you have to add the two folders to the PYTHONPATH.

If you are using the rsdl *repository*, you have to add the rsdl subfolder of the rsdl repository to the PYTHONPATH.

```
export PYTHONPATH=${PYTHONPATH}:.:[path/to/pypy]:[path/to/rsdl]
```

### Setting the SDL Driver
For testing the basic functionality of the VM you might want to disable the UI. You can do so by setting the SDL_VIDEODRIVER environment variable to dummy.

```
export SDL_VIDEODRIVER=dummy
```

### Building & Tests
Execute the following commands inside the main directory of this repository.

To build the VM:

```
[path to pypy repository]/rpython/bin/rpython targetimageloadingsmalltalk.py
```

To build the VM with enabled just-in-time compiler:

```
[path to pypy repository]/rpython/bin/rpython -Ojit targetimageloadingsmalltalk.py
```

To run the tests (most do not require building):

```
[path to pypy repository]/pytest.py [--slow|--quick] spyvm/test
```

### Starting an image
The build process will produce an executable called rsqueak.
The ```image/``` directory contains two images you can open with the following.
Use ```--help``` to see command line switches.

```
./rsqueak images/mini.image
./rsqueak images/Squeak4.5-noBitBlt.image
```




STM-enabled Rsqueak
===
This is a branch of RSqueak which incorporates the RPython STM transformation. Most of the initial code base comes from the results of a [project seminar](https://bitbucket.org/amintos/lang-smalltalk). The stm-gc-c7 branch is based on this version and the 64bit branch.

Setup for stm-enabled RSqueak
---
You can see the current state of the integration of the RPython STM in the stm-gc-c7 branch.
Beware that you can only build this branch if you have a 64-bit Linux. To build this branch you have to setup several things:

1. Change your local pypy repository to the stm-gc7 branch, commit dd3c06b
2. Get a clang which has ([the required patches](https://bitbucket.org/pypy/stmgc/src/d164a5bcad5e7615b4362b6a1a49d51e2e06de0c/c7/llvmfix/?at=default)). If you have a Debian-based OS you can use the following package: [llvm-pypystm](https://launchpad.net/~malte.swart/+archive/ubuntu/llvm-pypystm).

To build, use the following command:
```
[path to pypy repository]/rpython/bin/rpython --gc=stmgc targetimageloadingsmalltalk.py
```
