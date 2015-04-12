#!/bin/bash
set -ex

function doIt {
    gem install travis --no-ri --no-rdoc
    travis login -g $GH_TOKEN
    travis restart --no-interactive -r timfel/RSqueak-MacOSXBuild
}

if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
    if [ "$BUILD_TYPE" == "build" ]; then
	if [ "$BUILD_ARCH" == "32bit" ]; then
	    if [ "$TRAVIS_BRANCH" == "master" ]; then
		doIt
	    fi
	    if [ "$TRAVIS_BRANCH" == "osx-build" ]; then
		doIt
	    fi
	fi
    fi
fi
