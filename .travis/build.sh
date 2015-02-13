#!/bin/sh
set -ex

sudo i386 chroot "$chroot" sh -c "
    cd $PWD &&
    echo \$(pwd) &&
    ls &&
    PYTHONPATH=\"$PYTHONPATH:pypy-pypy/:pypy-rsdl/:.\"\
            python2.7 pypy-pypy/rpython/bin/rpython --batch -Ojit targetimageloadingsmalltalk.py"
