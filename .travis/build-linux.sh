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
    arm)
	binary=rsqueak-arm
	armv=$(schroot -c precise_arm -- uname -m)
	export SB2=$PWD/raspbian_arm
	export SB2OPT='-t ARM'
	# uses the 32-bit pypy from download_dependencies.py
	.build/pypy-linux32/bin/pypy .build/build.py --gc=incminimark --gcrootfinder=shadowstack --jit-backend=arm -Ojit --platform=arm
	cp rsqueak* rsqueak-$armv-Linux-jit-$TRAVIS_COMMIT
	buildcode=$?
	exitcode=$buildcode
	;;
    *) exit 0 ;;
esac

if [ $buildcode -eq 0 ]; then
    if [ "$TRAVIS_BRANCH" == "master" ]; then
        if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
            curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/ || true
            curl -T rsqueak-$armv* http://www.lively-kernel.org/babelsberg/RSqueak/ || true
	    if [ "$latest" == "true" ]; then
		# only builds that pass the jittests are 'latest'
		cp rsqueak-x86* rsqueak-linux-latest
		curl -T rsqueak-linux-latest http://www.lively-kernel.org/babelsberg/RSqueak/
	    fi
	fi
    fi
fi
exit $exitcode
