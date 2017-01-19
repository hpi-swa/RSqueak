#!/bin/bash
set -e

UNAME=darwin
EX=
#set EX to sudo if required.

exitcode=0
plugins=""
plugins_suffix=""
if [[ -n "$PLUGINS" ]]; then
  plugins="--plugins=${PLUGINS}"
  plugins_suffix="-${PLUGINS}"
fi

travis_wait() {
  local timeout=20

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

case "$BUILD_ARCH" in
  32bit)
    python .build/build.py $plugins
    exitcode=$?
    cp rsqueak rsqueak-x86-${UNAME}$plugins_suffix-jit-$TRAVIS_COMMIT || true
    # python .build/jittests.py
    # $EX rm -rf .build/pypy/rpython/_cache
    ;;
  64bit)
    travis_wait pypy .build/build.py -- $plugins
    exitcode=$?
    cp rsqueak rsqueak-x86_64-${UNAME}$plugins_suffix-jit-$TRAVIS_COMMIT || true
    # python .build/jittests.py
    # $EX rm -rf .build/pypy/rpython/_cache
    ;;
  lldebug)
    python .build/build.py --lldebug -Ojit
    exitcode=$?
    cp rsqueak rsqueak-x86-${UNAME}-dbg-$TRAVIS_COMMIT || true
    # $EX rm -rf .build/pypy/rpython/_cache
    ;;
  *) exit 0 ;;
esac

exit $exitcode
