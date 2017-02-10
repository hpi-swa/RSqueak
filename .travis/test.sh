#!/bin/sh
set -ex

testscript="unittests.py"

case "${TEST_TYPE}" in
  default) testflag="-s -S" ;;
  quick) testflag="-s -Q" ;;
  slow) testflag="-s -S" ;;
  coverage) testflag="-s -v -S --cov=rsqueakvm --cov-append " ;;
  plugin)
    testscript="plugintests.py"
    testflag="--plugin=${TEST_PLUGIN}"
    ;;
  *)
    echo "Wrong TEST_TYPE value (${TEST_TYPE}), not executing tests"
    exit 0
    ;;
esac

case "${BUILD_ARCH}" in
  32bit) testflag="${testflag} --32bit" ;;
  *) ;;
esac

ex="python"
if [[ "${TRAVIS_OS_NAME}" == "osx" ]] && [[ "${BUILD_ARCH}" == "64bit" ]]; then
  ex="pypy"
fi

${ex} ".build/${testscript}" ${testflag}
