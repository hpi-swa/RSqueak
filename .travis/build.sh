#!/bin/bash
set -ex

case "$BUILD_ARCH" in
32bit)
    binary=rsqueak
    sudo i386 chroot "$chroot" sh -c "
    cd $PWD &&
    python2.7 .build/build.py"
    cp rsqueak* rsqueak-x86-Linux-jit-$TRAVIS_COMMIT || true
    exitcode=$?
    ;;
64bit)
    binary=rsqueak-64
    python2.7 .build/build.py
    cp rsqueak* rsqueak-x86_64-Linux-jit-$TRAVIS_COMMIT || true
    exitcode=$?
    ;;
arm)
    binary=rsqueak-arm
    EXECUTE_JITTESTS=no
    armv=$(schroot -c precise_arm -- uname -m)
    export SB2=$PWD/precise_arm
    export SB2OPT='-t ARM'
    pypy-linux/bin/pypy .build/build.py --gc=incminimark --gcrootfinder=shadowstack --jit-backend=arm -Ojit --platform=arm
    cp rsqueak* rsqueak-$armv-Linux-jit-$TRAVIS_COMMIT || true
    exitcode=$?
    ;;
*) exit 0 ;;
esac

if [ $exitcode -eq 0 ]; then
    if [ "$TRAVIS_BRANCH" == "master" ]; then
        if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
	    case "$BUILD_ARCH" in
		32bit)
		    curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/
		    cp rsqueak-x86* rsqueak-linux-latest
		    curl -T rsqueak-linux-latest http://www.lively-kernel.org/babelsberg/RSqueak/
		    ;;
		arm)
		    curl -T rsqueak-$armv* http://www.lively-kernel.org/babelsberg/RSqueak/
		    cp rsqueak-$armv* rsqueak-linux-arm-latest
		    curl -T rsqueak-linux-arm-latest http://www.lively-kernel.org/babelsberg/RSqueak/
		    ;;
	    esac
	fi
    fi
    sudo rm -rf .build/pypy/rpython/_cache
    if [ yes == "$EXECUTE_JITTESTS" ]; then
		python2.7 .build/jittests.py
		exit $?
    fi
else
    exit $exitcode
fi
