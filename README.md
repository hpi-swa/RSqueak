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
There are two branches integrating the RPython STM into SPY: stm-c4,
storage-stm-c4. You have to change two things of the setup to build those
branches.

1. Change your local pypy repository to the stm-c4 branch.
2. Build using the following command:
```
[path to pypy repository]/rpython/bin/rpython --gc=stmgc [path to lang-smalltalk
repository]/targetimageloadingsmalltalk.py
```
