#!/bin/sh
set -ex

case "$TEST_TYPE" in
default) testflag="" ;;
quick) testflag="-Q" ;;
slow) testflag="-S" ;;
*) echo "Wrong TEST_TYPE value ($TEST_TYPE), not executing tests"
   exit 0 ;;
esac

python .build/unittests.py -s $testflag
