#!/bin/bash
set -e

python .build/build.py

cp rsqueak rsqueak-x86-darwin-jit-$TRAVIS_COMMIT
curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/
curl -T rsqueak-x86* -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/

if [ "$TRAVIS_BRANCH" == "master" ]; then
    if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
	cp rsqueak rsqueak-darwin-latest
	curl -T rsqueak-darwin-latest http://www.lively-kernel.org/babelsberg/RSqueak/
	curl -T rsqueak-darwin-latest -u "$DEPLOY_CREDENTIALS" https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/
    fi
fi
