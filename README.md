Spy
=========

A Squeak VM written in RPython, called "SPy VM".

Setup
----
### Required Projects
You need three repositories: 

* This one
* pypy/pypy
* pypy/rsdl

### Required packages
You need the following packages on your OS. Install with your favorite package
manager:
* pypy (For faster translation of the SPY VM)
* libsdl-dev

### Adjusting the PYTHONPATH
In order to allow the RPython toolchain to find the rsdl module you have to add
the rsdl folder to the PYTHONPATH. Note that you have to add the rsdl subfolder
of the rsdl repository to the PYTHONPATH.

```
export PYTHONPATH=${PYTHONPATH}:[path to rsdl repository]/rsdl
```

### Setting the SDL Driver
For testing the basic functionality of the VM it is currently best to disable
the UI. You can do so by setting the SDL_VIDEODRIVER environment variable to
dummy.
```
export SDL_VIDEODRIVER=dummy
```

### Building
To build the VM enter the following:

```
[path to pypy repository]/rpython/bin/rpython [path to lang-smalltalk
repository]/targetimageloadingsmalltalk.py
```

To build the VM with enabled just-in-time compiler:
```
[path to pypy repository]/rpython/bin/rpython -O jit [path to lang-smalltalk
repository]/targetimageloadingsmalltalk.py
```

### Starting an image
The build process will produce an executable e.g. called
targetimageloadingsmalltalk-c. Start it with the following:
```
./targetimageloadingsmalltalk-c images/Squeak4.5-*.image
```

Setup for stm-enabled SPY
---
You can see the current state of the integration of the RPython STM in our stmgc-c7 branch.
Beware that you can only build this branch if you have a 64-bit linux. To build this branch you have to setup several things:

1. Change your local pypy repository to the stm-gc7 branch, commit dd3c06b
2. Get a clang which has the patches from ([Clang patches](https://bitbucket.org/pypy/stmgc/src/d164a5bcad5e7615b4362b6a1a49d51e2e06de0c/c7/llvmfix/?at=default)). If you have a Debian-based OS you can use the following package: https://launchpad.net/~malte.swart/+archive/ubuntu/llvm-pypystm

To build, use the following command:
```
[path to pypy repository]/rpython/bin/rpython --gc=stmgc [path to lang-smalltalk
repository]/targetimageloadingsmalltalk.py
```