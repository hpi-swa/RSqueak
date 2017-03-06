#!/bin/sh
set -ex

case "${TEST_TYPE}" in
  default) testflag="-S" ;;
  quick) testflag="-Q" ;;
  slow) testflag="-S" ;;
  coverage) testflag="-v -S --cov=rsqueakvm --cov-append " ;;
  *)
    echo "Wrong TEST_TYPE value (${TEST_TYPE}), not executing tests"
    exit 0
    ;;
esac

case "${BUILD_ARCH}" in
  32bit) testflag="${testflag} --32bit" ;;
  *) ;;
esac

pypy ".build/unittests.py" -s ${testflag}
