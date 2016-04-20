#!/bin/bash
set -e

UNAME=darwin
EX=
#set EX to sudo if required.

case "$BUILD_ARCH" in
    32bit)
        binary=rsqueak
        python .build/build.py
        buildcode=$?  ;   exitcode=$buildcode
        cp rsqueak rsqueak-x86-${UNAME}-jit-$TRAVIS_COMMIT || true
        # python .build/jittests.py
        # $EX rm -rf .build/pypy/rpython/_cache
        # exitcode=$?
        # if [ $exitcode -eq 0 ]; then latest=true; fi
        ;;
    64bit)
        binary=rsqueak
        python .build/build.py 64bit
        buildcode=$?  ;   exitcode=$buildcode
        cp rsqueak rsqueak-x86_64-${UNAME}-jit-$TRAVIS_COMMIT || true
        # python .build/jittests.py
        # $EX rm -rf .build/pypy/rpython/_cache
        # exitcode=$?
        # if [ $exitcode -eq 0 ]; then latest=true; fi
        ;;
    lldebug)
        binary=rsqueak
        python .build/build.py --lldebug -Ojit
        cp rsqueak rsqueak-x86-${UNAME}-dbg-$TRAVIS_COMMIT || true
        exitcode=$?
        buildcode=$exitcode
        # $EX rm -rf .build/pypy/rpython/_cache
        ;;
    *) exit 0 ;;
esac

if [ $buildcode -eq 0 ]; then
    curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/
    curl -T rsqueak-x86* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/

    if [ "$TRAVIS_BRANCH" == "master" -a "$TRAVIS_PULL_REQUEST" == "false" ]; then
	case "$BUILD_ARCH" in
	    32bit)
		cp rsqueak rsqueak-${UNAME}-latest
		curl -T rsqueak-${UNAME}-latest http://www.lively-kernel.org/babelsberg/RSqueak/
		curl -T rsqueak-${UNAME}-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/
		;;
	    64bit)
		cp rsqueak rsqueak-${UNAME}-x86_64-latest
		curl -T rsqueak-${UNAME}-x86_64-latest http://www.lively-kernel.org/babelsberg/RSqueak/
		curl -T rsqueak-${UNAME}-x86_64-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/
		;;
	esac
    fi
fi

exit $buildcode
