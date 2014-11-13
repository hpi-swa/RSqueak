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
    * For faster translation of the SPY VM. Alternatively use default Python.
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
The build process will produce an executable e.g. called targetimageloadingsmalltalk-c. Start it with the following:

```
./targetimageloadingsmalltalk-c images/Squeak4.5-*.image
```

Setup for stm-enabled SPY
---

There are two branches integrating the RPython STM into SPY: stm-c4, storage-stm-c4. You have to change two things of the setup to build those branches.

1. Change your local pypy repository to the stm-c4 branch.
2. Add the ```--gc=stmgc``` when building:
```
[path to pypy repository]/rpython/bin/rpython --gc=stmgc targetimageloadingsmalltalk.py
```
