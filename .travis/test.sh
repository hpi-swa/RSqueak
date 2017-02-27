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
    testflag="--plugins=${PLUGINS} --plugin-dir=${PLUGIN_DIR}"
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

pypy ".build/${testscript}" ${testflag}
