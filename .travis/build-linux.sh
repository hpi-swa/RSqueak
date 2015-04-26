#!/bin/bash
set -ex

binary=rsqueak
python .build/build.py
cp rsqueak* rsqueak-x86-Linux-jit-$TRAVIS_COMMIT || true
exitcode=$?

if [ $exitcode -eq 0 ]; then
    if [ "$TRAVIS_BRANCH" == "master" ]; then
        if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
            curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/
            cp rsqueak-x86* rsqueak-linux-latest
            curl -T rsqueak-linux-latest http://www.lively-kernel.org/babelsberg/RSqueak/
	fi
    fi
    sudo rm -rf .build/pypy/rpython/_cache

    pypy .build/jittests.py
    exitcode=$?
fi
exit $exitcode
