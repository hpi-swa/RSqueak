#!/bin/sh
set -ex

if [ "$TEST_TYPE" == "64bitdefault" ]; then
    echo $(pwd)
    ls
    PYTHONPATH="$PYTHONPATH:pypy-pypy/:pypy-rsdl/:."\
            python2.7 pypy-pypy/pytest.py -s spyvm/test/
    exit $?
else
    case "$TEST_TYPE" in
       default) testflag="" ;;
       quick) testflag="-Q" ;;
       slow) testflag="-S" ;;
       *) echo "Wrong TEST_TYPE value ($TEST_TYPE), not executing tests"
          exit 0 ;;
    esac

    sudo i386 chroot "$chroot" sh -c "
        cd $PWD &&
        echo \$(pwd) &&
        ls &&
        PYTHONPATH=\"$PYTHONPATH:pypy-pypy/:pypy-rsdl/:.\"\
            python2.7 pypy-pypy/pytest.py -s $testflag spyvm/test/"
fi
