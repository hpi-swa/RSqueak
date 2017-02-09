Development
======================================

The `.build` directory includes several scripts that make development
easier. Once you've setup your system for building, you can use these
to work on the RSqueak/VM.

run.py
----------

This script executes RSqueak/VM in *hosted* mode, that is, it runs on
top of a Python interpreter. This is very slow (we recommend using PyPy),
but it can be useful to debug specific aspects of the VM quickly. Ideally,
you set up an image so that it executes the code that you are interested in
early during startup (`DisplayScreen class>>startUp` is a good candidate)
and then you add your breakpoints to the source. You can also pass commandline
arguments to the script or tweak the default arguments in the script itself.

*Running a "Smalltalk REPL"*

When you want to work on primitives or plugins, it is useful to prepare an image
a little (for example, make it so test runs print to the console), and then run
the following:

.. code-block:: bash

  pypy .build/run.py --shell <PATH_TO_IMAGE>

This will load the image and dump you in a simple REPL for Smalltalk, but with
some commands to help you execute Python code and, in particular, to reload the
Python code you write in any plugin or primitive file. Thus, you can run some
Smalltalk code, check for errors, change the primitive code, reload it, and try
again. This avoids having to reload the image in interpreted mode all the time
(which can be slow).

There is an integrated help that you can get by typing !help, and there is
limited autocomplete, too.

Note that the REPL only loads the image, but does not process startup. For many
things, you might want to run `FileStream startUp: true` or `Delay startUp` to
get basic I/O working or delays working.

unittests.py
-------------

The second script that is useful for working on issues regarding the
interpreter is `unittests.py`. By default it runs all tests under the
`rsqueakvm/test` directory (but not those in `rsqueakvm/test/jittest/`). This
is a standalone pytest script, so you can pass arguments or select single
test files as you would for pytest.

jittests.py
------------

This script requires that you have already built an `rsqueak` binary and
that you have the C Squeak VM installed. It executes the tests in
`rsqueakvm/test/jittest/` and checks for the JIT output. We use these tests to
ensure that development on the VM does not break JIT optimizations.

jit.py
-------

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

shell.py
---------

This script sets all the environment variables as if for translating the VM,
and the drops you into an interactive Python prompt. You can type `terminal()`
to drop into a system shell (`%COMSPEC%` on Windows `$SHELL` or `/bin/sh` on
Unices) which then has all the environment variables set up that you need to
do manual partial translations or similar things.
