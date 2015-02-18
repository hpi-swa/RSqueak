#!/bin/bash
set -ex

if [ "$TEST_TYPE" == "build" ]; then
    sudo i386 chroot "$chroot" sh -c "
    cd $PWD &&
    echo \$(pwd) &&
    ls &&
    PYTHONPATH=\"$PYTHONPATH:pypy-pypy/:pypy-rsdl/:.\"\
            python2.7 pypy-pypy/rpython/bin/rpython --batch -Ojit targetrsqueak.py"
    mv rsqueak* rsqueak-x86-Linux-jit-$TRAVIS_COMMIT || true
    exitcode=$?
else if [ "$TEST_TYPE" == "64bitbuild" ]; then
    echo $(pwd)
    ls
    PYTHONPATH="$PYTHONPATH:pypy-pypy/:pypy-rsdl/:." python2.7 \
            pypy-pypy/rpython/bin/rpython --batch -Ojit targetimageloadingsmalltalk.py
    mv rsqueak* rsqueak-x86_64-Linux-jit-$TRAVIS_COMMIT || true
    exitcode=$?
else
    exit 0
fi fi

if [ $exitcode -eq 0 ]; then
    if [ "$TRAVIS_BRANCH" == "master" ]; then
        if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
            curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/
	fi
    fi
fi
exit $exitcode
