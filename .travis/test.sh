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
    echo $(pwd)
    ls
    PYTHONPATH="$PYTHONPATH:pypy-pypy/:pypy-rsdl/:."\
            python2.7 pypy-pypy/pytest.py -s $testflag spyvm/test/
    exit $?
    ;;
32bit)
    sudo i386 chroot "$chroot" sh -c "
        cd $PWD &&
        echo \$(pwd) &&
        ls &&
        PYTHONPATH=\"$PYTHONPATH:pypy-pypy/:pypy-rsdl/:.\"\
            python2.7 pypy-pypy/pytest.py -s $testflag spyvm/test/"
    ;;
*) exit 0 ;;
esac

