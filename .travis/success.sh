#!/bin/bash
set -e

case "$TRAVIS_OS_NAME" in
  linux)
    curl -T rsqueak-x86* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/sqpyte/ || true
    ;;
  osx)
    curl -T rsqueak-x86* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/sqpyte/ || true
    ;;
esac
