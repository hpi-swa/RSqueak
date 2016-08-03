#!/bin/bash
set -e

UNAME=darwin
EX=
#set EX to sudo if required.

# set up PKG_CONFIG_PATH in order to use openssl installed via Homebrew
PKG_CONFIG_PATH=$(brew --prefix)/opt/openssl/lib/pkgconfig

exitcode=0
plugins=""
plugins_suffix=""
if [[ -n "$PLUGINS" ]]; then
  plugins="--plugins=${PLUGINS}"
  plugins_suffix="-${PLUGINS}"
fi

case "$BUILD_ARCH" in
  32bit)
    python .build/build.py $plugins
    exitcode=$?
    cp rsqueak rsqueak-x86-${UNAME}$plugins_suffix-jit-$TRAVIS_COMMIT || true
    # python .build/jittests.py
    # $EX rm -rf .build/pypy/rpython/_cache
    ;;
  64bit)
    python .build/build.py -- --64bit $plugins
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
