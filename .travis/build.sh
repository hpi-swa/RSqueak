#!/bin/bash
set -e

readonly BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Only build arm on master
if [[ "${TRAVIS_BRANCH}" != "master" ]] && [[ "${BUILD_ARCH}" = arm* ]]; then
    exit 0
fi

travis_wait() {
  local timeout=110

  local cmd="$@"

  $cmd &
  local cmd_pid=$!

  travis_jigger $! $timeout $cmd &
  local jigger_pid=$!
  local result

  {
    wait $cmd_pid 2>/dev/null
    result=$?
    ps -p$jigger_pid &>/dev/null && kill $jigger_pid
  }

  echo -e "\nThe command $cmd exited with $result."
  return $result
}

travis_jigger() {
  # helper method for travis_wait()
  local cmd_pid=$1
  shift
  local timeout=$1 # in minutes
  shift
  local count=0

  # clear the line
  echo -e "\n"

  while [ $count -lt $timeout ]; do
    count=$(($count + 1))
    echo -ne "."
    sleep 60
  done

  echo -e "\nTimeout (${timeout} minutes) reached. Terminating \"$@\"\n"
  kill -9 $cmd_pid
}

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
    SCRIPT_NAME="build-osx.sh"
    ;;
esac

if [[ -n "${TEST_TYPE}" ]]; then
  SCRIPT_NAME="test.sh"
fi

travis_wait "${BASE}/${SCRIPT_NAME}"

if [[ "${PLUGINS}" = *"database_plugin"* ]]; then
  "${BASE}/test_database_integration.sh"
fi
