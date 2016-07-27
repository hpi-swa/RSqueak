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

case "$BUILD_ARCH" in
  64bit) testflag="$testflag --64bit" ;;
  *) ;;
esac

python .build/unittests.py -s $testflag
