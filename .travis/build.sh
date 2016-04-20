#!/bin/bash
set -ex

readonly BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"


if [[ -n "${TEST_TYPE}" ]]; then
  "${BASE}/test.sh"
  exit
fi

case "$TRAVIS_OS_NAME" in
  linux)
    export SDL_VIDEODRIVER=dummy;
    "${BASE}/build-linux.sh"
    ;;
  osx)
    if [[ "$BUILD_ARCH" == 32bit -o "$BUILD_ARCH" == lldebug ]]; then
      export CFLAGS="-arch i386";
      export CC="cc -arch i386";
      export VERSIONER_PYTHON_PREFER_32_BIT=yes;
    fi
    "${BASE}/build-osx.sh"
    ;;
esac