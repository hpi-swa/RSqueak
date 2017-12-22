Building from Source
=====================

Common to all systems
----------------------

We have scripts for installing dependencies, building, running the unit tests,
and running JIT tests in the `.build` subdirectory. You need the `PyPy` source
tree, and the `rsdl` source tree. If you already have all these, run
`.build/build.py --32bit` to build 32-bit VM. Run `.build/build.py` to compile a
64-bit VM. The script passes all other arguments on to the `rpython` translator,
so you can pass other options.

It will generate a config file `.build/buildconfig.ini` where you can set your
paths. You can also run `.build/download_dependencies.py` to download the
dependencies automatically. You will also need a Python and a C compiler for
32-bit compilation, if you plan to do 32-bit development.

Windows
--------

On Windows, you will have to use the C compiler that comes with Visual Studio
2008, because newer ones crash the JIT. It suffices to just install the
`Microsoft C compiler V90 plus Windows SDK 7`_.
Also, install the `Python 2.7 Visual Studio compiler package`_.
The packages provided in the link install it to the default paths. If you
already have the compiler and SDK, you can also just update the paths in
`.build/buildconfig.ini`.

.. _Microsoft C compiler V90 plus Windows SDK 7: https://github.com/hpi-swa/RSqueak/releases/download/Dependencies/vc_stdx86.zip
.. _Python 2.7 Visual Studio compiler package: https://aka.ms/vcpython27

Linux
------

RSqueak/VM can currently be compiled in both 32-bit and 64-bit
configurations. For 32-bit, you need to use 32-bit python and SDL2 using 32-bit
libraries for everything. The easiest way to ensure that is to use a chroot, but
you can also install the `:i386` versions of the SDL2 dependencies for your
distro.

macOS
---------

To compile RSqueak/VM for 32-bit, run

.. code-block:: bash

  export VERSIONER_PYTHON_PREFER_32_BIT=yes

before you run any of the python scripts in the `.build` directory. You also
need to download SDL2 as a framework (homebrew version is not tested). Check
the `.travis/build-osx.sh` if you get stuck anywhere.