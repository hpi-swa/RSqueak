#!/bin/bash
set -e

# Only build arm on master
if [[ "${TRAVIS_BRANCH}" != "master" ]] && [[ "${BUILD_ARCH}" = arm* ]]; then
  exit 0
fi

if [[ -n "${TEST_TYPE}" ]]; then
  if [[ "${TEST_TYPE}" = "coverage" ]]; then
    export PATH=.build/pypy-linux32/bin/:$PATH;
    coveralls;
  fi
  exit
fi

case "$TRAVIS_OS_NAME" in
  linux)
    UNAME=linux
    armv="${BUILD_ARCH}raspbian"
    curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/ || true
    curl -T rsqueak-$armv* http://www.lively-kernel.org/babelsberg/RSqueak/ || true
    curl -T rsqueak-x86* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/ || true
    curl -T rsqueak-$armv* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/ || true
    if [[ "$BUILD_ARCH" = "32bit" ]]; then
      curl -v -H "commitid: $TRAVIS_COMMIT" -H "branch: $TRAVIS_BRANCH" -X POST http://lively-kernel.org/codespeed/ || true
    fi
    if [[ "$TRAVIS_BRANCH" = "master" ]] && [[ "$TRAVIS_PULL_REQUEST" = "false" ]]; then
      case "$BUILD_ARCH" in
        32bit)
          # only builds that pass the jittests are 'latest'
          cp rsqueak-x86* rsqueak-${UNAME}-latest
          curl -T rsqueak-${UNAME}-latest http://www.lively-kernel.org/babelsberg/RSqueak/
          curl -T rsqueak-${UNAME}-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/ || true
          ;;
        64bit)
          cp rsqueak-x86_64* rsqueak-${UNAME}-x86_64-latest
          curl -T rsqueak-${UNAME}-x86_64-latest http://www.lively-kernel.org/babelsberg/RSqueak/
          curl -T rsqueak-${UNAME}-x86_64-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/ || true
          ;;
        arm*)
          cp rsqueak-$armv* rsqueak-${UNAME}-$armv-latest
          curl -T rsqueak-${UNAME}-$armv-latest http://www.lively-kernel.org/babelsberg/RSqueak/
          curl -T rsqueak-${UNAME}-$armv-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/ || true
          ;;
      esac
    fi
    ;;
  osx)
    UNAME=darwin
    curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/
    curl -T rsqueak-x86* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/
    if [[ "$TRAVIS_BRANCH" = "master" ]] && [[ "$TRAVIS_PULL_REQUEST" = "false" ]]; then
      case "$BUILD_ARCH" in
        32bit)
          cp rsqueak rsqueak-${UNAME}-latest
          curl -T rsqueak-${UNAME}-latest http://www.lively-kernel.org/babelsberg/RSqueak/
          curl -T rsqueak-${UNAME}-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/
          ;;
        64bit)
          cp rsqueak rsqueak-${UNAME}-x86_64-latest
          curl -T rsqueak-${UNAME}-x86_64-latest http://www.lively-kernel.org/babelsberg/RSqueak/
          curl -T rsqueak-${UNAME}-x86_64-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/
          ;;
      esac
    fi
    ;;
esac
