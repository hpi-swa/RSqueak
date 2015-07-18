#!/bin/bash
set -ex

case "$BUILD_ARCH" in
    32bit)
	binary=rsqueak
	python .build/build.py
	cp rsqueak* rsqueak-x86-Linux-jit-$TRAVIS_COMMIT || true
	buildcode=$?
	python .build/jittests.py
	sudo rm -rf .build/pypy/rpython/_cache
	exitcode=$?
	if [ $exitcode -eq 0 ]; then latest=true; fi
	;;
    armv6)
	binary=rsqueak-arm
	armv=armv6raspbian
	export SB2OPT="-t ${SB2NAME}"
	export CFLAGS="-march=armv6 -mfpu=vfp -mfloat-abi=hard -marm\
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
	cp rsqueak* rsqueak-$armv-Linux-jit-$TRAVIS_COMMIT
	buildcode=$?
	exitcode=$buildcode
	latest=true
	;;
    armv7)
	binary=rsqueak-arm
	armv=$(schroot -c precise_arm -- uname -m)
	export SB2=$PWD/precise_arm
	export SB2OPT='-t ARM'
	# uses the 32-bit pypy from download_dependencies.py
	.build/pypy-linux32/bin/pypy .build/build.py --gc=incminimark --gcrootfinder=shadowstack --jit-backend=arm -Ojit --platform=arm
	cp rsqueak* rsqueak-$armv-Linux-jit-$TRAVIS_COMMIT
	buildcode=$?
	exitcode=$buildcode
	latest=true
	;;
    *) exit 0 ;;
esac

if [ $buildcode -eq 0 ]; then
	curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/ || true
	curl -T rsqueak-$armv* http://www.lively-kernel.org/babelsberg/RSqueak/ || true
	if [ "$BUILD_ARCH" == "32bit" ]; then
		curl -v -H "commitid: $TRAVIS_COMMIT" -X POST http://lively-kernel.org/codespeed/ || true
	fi
    if [ "$TRAVIS_BRANCH" == "master" ]; then
        if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
			if [ "$latest" == "true" ]; then
	    		if [ "$BUILD_ARCH" == "32bit" ]; then
					# only builds that pass the jittests are 'latest'
					cp rsqueak-x86* rsqueak-linux-latest
					curl -T rsqueak-linux-latest http://www.lively-kernel.org/babelsberg/RSqueak/
				else
					cp rsqueak-$armv* rsqueak-linux-$armv-latest
					curl -T rsqueak-linux-$armv-latest http://www.lively-kernel.org/babelsberg/RSqueak/
				fi
			fi
		fi
    fi
fi
exit $exitcode
exit $exitcode
