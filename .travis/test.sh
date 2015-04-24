#!/bin/sh
set -ex

case "$TEST_TYPE" in
default) testflag="" ;;
quick) testflag="-Q" ;;
slow) testflag="-S" ;;
*) echo "Wrong TEST_TYPE value ($TEST_TYPE), not executing tests"
   exit 0 ;;
esac

case "$BUILD_ARCH" in
64bit)
    python2.7 .build/unittests.py -s $testflag
    exit $?
    ;;
32bit)
    sudo i386 chroot "$chroot" sh -c "
        cd $PWD &&
        python2.7 .build/unittests.py -s $testflag"
    ;;
*) exit 0 ;;
esac

