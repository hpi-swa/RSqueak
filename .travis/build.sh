#!/bin/bash
set -ex

sudo i386 chroot "$chroot" sh -c "
    cd $PWD &&
    echo \$(pwd) &&
    ls &&
    PYTHONPATH=\"$PYTHONPATH:pypy-pypy/:pypy-rsdl/:.\"\
            python2.7 pypy-pypy/rpython/bin/rpython --batch -Ojit targetrsqueak.py"

exitcode=$?
if [ $exitcode -eq 0 ]; then
    if [ "$TRAVIS_BRANCH" == "master" ]; then
	if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
	    mv rsqueak rsqueak-x86-Linux-jit-$TRAVIS_COMMIT
	    curl -T rsqueak-x86-Linux-jit-* http://www.lively-kernel.org/babelsberg/RSqueak/
	fi
    fi
fi
exit $exitcode
