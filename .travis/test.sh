#!/bin/sh
set -ex

case "${TEST_TYPE}" in
  default) testflag="" ;;
  quick) testflag="-Q" ;;
  slow) testflag="-S" ;;
  coverage) testflag="-Q --cov=rsqueakvm --cov-append" ;;
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

${ex} .build/unittests.py -s ${testflag}
