#!/bin/bash
set -ex

case "$BUILD_ARCH" in
32bit)
    binary=rsqueak
    sudo i386 chroot "$chroot" sh -c "
    cd $PWD &&
    echo \$(pwd) &&
    ls &&
    PYTHONPATH=\"$PYTHONPATH:pypy-pypy/:pypy-rsdl/:.\"\
            python2.7 pypy-pypy/rpython/bin/rpython --batch -Ojit targetrsqueak.py"
    cp rsqueak* rsqueak-x86-Linux-jit-$TRAVIS_COMMIT || true
    exitcode=$?
    ;;
64bit)
    binary=rsqueak-64
    echo $(pwd)
    ls
    PYTHONPATH="$PYTHONPATH:pypy-pypy/:pypy-rsdl/:." python2.7 \
            pypy-pypy/rpython/bin/rpython --batch -Ojit targetrsqueak.py
    cp rsqueak* rsqueak-x86_64-Linux-jit-$TRAVIS_COMMIT || true
    exitcode=$?
    ;;
*) exit 0 ;;
esac

if [ $exitcode -eq 0 ]; then
    if [ "$TRAVIS_BRANCH" == "master" ]; then
        if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
            curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/
	fi
    fi
    sudo rm -rf pypy-pypy/rpython/_cache
    if [ yes == "$EXECUTE_JITTESTS" ]; then
	case "$BUILD_ARCH" in
	    32bit)
		sudo i386 chroot "$chroot" sh -c "
                cd $PWD &&
                echo \$(pwd) &&
                ls &&
                PYTHONPATH=\"$PYTHONPATH:pypy-pypy/:pypy-rsdl/:.\"\
                    python2.7 pypy-pypy/pytest.py --jit=./$binary spyvm/test/jittest/"
		exitcode=$?
		;;
	    64bit)
		PYTHONPATH="$PYTHONPATH:pypy-pypy/:pypy-rsdl/:." python2.7 \
		    pypy-pypy/pytest.py --jit=./$binary spyvm/test/jittest/
		exitcode=$?
		;;
	    *)
		exitcode=0
		;;
	esac
        exit $exitcode
    fi
else
    exit $exitcode
fi
