#!/bin/bash
# set -ex

if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
    if [ "$BUILD_TYPE" == "build" ]; then
	if [ "$BUILD_ARCH" == "32bit" ]; then
	    if [ "$TRAVIS_BRANCH" == "master" ]; then
		gem install travis --no-ri --no-rdoc
		travis login --no-interactive -g $GH_TOKEN
		travis restart --no-interactive -r $OSXREPO
	    fi
	fi
    fi
fi
