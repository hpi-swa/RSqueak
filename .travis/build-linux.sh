#!/bin/bash
set -e

UNAME=linux
EX=
#set EX to sudo if required.

exitcode=0

python .build/build.py 64bit
exitcode=$?
cp rsqueak rsqueak-x86_64-${UNAME}-jit-$TRAVIS_COMMIT || true
# python .build/jittests.py 64bit
$EX rm -rf .build/pypy/rpython/_cache

exit $exitcode
