#!/bin/bash
set -e

case "$TRAVIS_OS_NAME" in
  linux)
    curl -T rsqueak-x86* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/sqpyte/ || true
    TRAVIS_MSG=$(git log -n1 --pretty="format:%s")
    if [[ "${TRAVIS_MSG#*skip bench}" != "${TRAVIS_MSG}" ]] || [[ "${TRAVIS_MSG#*bench skip}" != "${TRAVIS_MSG}" ]]; then
      curl -v -H "commitid: $TRAVIS_COMMIT" -H "branch: $TRAVIS_BRANCH" -H "vm: rsqueak64" -X POST http://lively-kernel.org/codespeed/ || true
    fi
    ;;
  osx)
    curl -T rsqueak-x86* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/sqpyte/ || true
    ;;
esac
