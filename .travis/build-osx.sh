#!/bin/bash
set -e

UNAME=darwin
EX=
#set EX to sudo if required.

exitcode=0

case "$BUILD_ARCH" in
  32bit)
    python .build/build.py
    exitcode=$?
    cp rsqueak rsqueak-x86-${UNAME}-jit-$TRAVIS_COMMIT || true
    # python .build/jittests.py
    # $EX rm -rf .build/pypy/rpython/_cache
    ;;
  64bit)
    python .build/build.py -- --64bit
    exitcode=$?
    cp rsqueak rsqueak-x86_64-${UNAME}-jit-$TRAVIS_COMMIT || true
    # python .build/jittests.py
    # $EX rm -rf .build/pypy/rpython/_cache
    ;;
  lldebug)
    python .build/build.py --lldebug -Ojit
    exitcode=$?
    cp rsqueak rsqueak-x86-${UNAME}-dbg-$TRAVIS_COMMIT || true
    # $EX rm -rf .build/pypy/rpython/_cache
    ;;
  *) exit 0 ;;
esac

exit $exitcode
