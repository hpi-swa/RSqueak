#!/bin/bash
set -e

UNAME=linux
EX=
#set EX to sudo if required.

case "$BUILD_ARCH" in
    32bit)
        binary=rsqueak
        python .build/build.py
        buildcode=$?
        cp rsqueak* rsqueak-x86-${UNAME}-jit-$TRAVIS_COMMIT || true
        python .build/jittests.py
        $EX rm -rf .build/pypy/rpython/_cache
        exitcode=$?
        if [ $exitcode -eq 0 ]; then latest=true; fi
        ;;
    64bit)
        binary=rsqueak
        python .build/build.py 64bit
        buildcode=$?
        cp rsqueak* rsqueak-x86_64-${UNAME}-jit-$TRAVIS_COMMIT || true
        python .build/jittests.py
        $EX rm -rf .build/pypy/rpython/_cache
        exitcode=$?
        if [ $exitcode -eq 0 ]; then latest=true; fi
        ;;
    lldebug)
        binary=rsqueak
        python .build/build.py --lldebug -Ojit
        cp rsqueak* rsqueak-x86-${UNAME}-dbg-$TRAVIS_COMMIT || true
        exitcode=$?
        buildcode=$exitcode
        $EX rm -rf .build/pypy/rpython/_cache
        ;;
    arm*)
        binary=rsqueak-arm
        armv="${BUILD_ARCH}raspbian"
        export SB2OPT="-t ${SB2NAME}"
        export CFLAGS="-march=$BUILD_ARCH -mfpu=vfp -mfloat-abi=hard -marm\
               -I${SB2}/usr/include/arm-linux-gnueabihf/"
        export LDFLAGS="-L${SB2}/usr/lib/arm-linux-gnueabihf/pulseaudio\
               -Wl,-rpath=${SB2}/usr/lib/arm-linux-gnueabihf/pulseaudio\
               -L${SB2}/usr/lib/arm-linux-gnueabihf/\
               -Wl,-rpath=${SB2}/usr/lib/arm-linux-gnueabihf/\
               -L${SB2}/lib/arm-linux-gnueabihf/\
               -Wl,-rpath=${SB2}/lib/arm-linux-gnueabihf/"
        # uses the 32-bit pypy from download_dependencies.py
        .build/pypy-linux32/bin/pypy .build/build.py --gc=incminimark --gcrootfinder=shadowstack --jit-backend=arm -Ojit --platform=arm || true
        # sometimes the translation fails because "make got killed", make sure
        oldpwd=$(pwd)
        cd /tmp/usession-default-0/testing_1/
        sb2 -t rasp make -j 5
        cp rsqueak $oldpwd/rsqueak
        cd $oldpwd
        cp rsqueak* rsqueak-$armv-${UNAME}-jit-$TRAVIS_COMMIT
        buildcode=$?
        exitcode=$buildcode
        latest=true
        ;;
    *) exit 0 ;;
esac

if [ $buildcode -eq 0 ]; then
    curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/ || true
    curl -T rsqueak-$armv* http://www.lively-kernel.org/babelsberg/RSqueak/ || true
    curl -T rsqueak-x86* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/ || true
    curl -T rsqueak-$armv* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/ || true
    if [ "$BUILD_ARCH" == "32bit" ]; then
        curl -v -H "commitid: $TRAVIS_COMMIT" -X POST http://lively-kernel.org/codespeed/ || true
    fi
    if [ "$TRAVIS_BRANCH" == "master" -a "$TRAVIS_PULL_REQUEST" == "false" -a "$latest" == "true" ]; then
        if [ "$BUILD_ARCH" == "32bit" ]; then
            # only builds that pass the jittests are 'latest'
            cp rsqueak-x86* rsqueak-${UNAME}-latest
            curl -T rsqueak-linux-latest http://www.lively-kernel.org/babelsberg/RSqueak/
            curl -T rsqueak-linux-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/ || true
        else
            cp rsqueak-$armv* rsqueak-${UNAME}-$armv-latest
            curl -T rsqueak-${UNAME}-$armv-latest http://www.lively-kernel.org/babelsberg/RSqueak/
            curl -T rsqueak-${UNAME}-$armv-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/ || true
        fi
    fi
fi
exit $exitcode
