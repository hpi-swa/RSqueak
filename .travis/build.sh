#!/bin/bash
set -e

readonly BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Only build arm on master
if [[ "${TRAVIS_BRANCH}" != "master" ]] && [[ "${BUILD_ARCH}" = arm* ]]; then
    exit 0
fi

case "$TRAVIS_OS_NAME" in
  linux)
    export SDL_VIDEODRIVER=dummy;
    SCRIPT_NAME="build-linux.sh"
    ;;
  osx)
    if [[ "$BUILD_ARCH" = 32bit ]] || [[ "$BUILD_ARCH" = lldebug ]]; then
      export CFLAGS="-arch i386";
      export CC="cc -arch i386";
      export VERSIONER_PYTHON_PREFER_32_BIT=yes;
    fi
    # set up PKG_CONFIG_PATH in order to use openssl installed via Homebrew
    export PKG_CONFIG_PATH=$(brew --prefix)/opt/openssl/lib/pkgconfig
    SCRIPT_NAME="build-macos.sh"
    ;;
esac

if [[ -n "${TEST_TYPE}" ]]; then
  SCRIPT_NAME="test.sh"
fi

"${BASE}/${SCRIPT_NAME}"

if [[ "${PLUGINS}" = *"database_plugin"* ]]; then
  "${BASE}/test_database_integration.sh"
fi