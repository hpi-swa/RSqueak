#!/bin/bash
set -ex

# install
curl -L -O https://bitbucket.org/pypy/pypy/get/default.tar.bz2
tar xjvf default.tar.bz2
mv pypy-pypy* pypy
rm default.tar.bz2
curl -L -O https://bitbucket.org/pypy/rsdl/get/default.tar.bz2
tar xjvf default.tar.bz2
mv pypy-rsdl* rsdl
rm default.tar.bz2
git clone --depth=1 https://github.com/HPI-SWA-Lab/RSqueak.git
mv RSqueak/* .
mv RSqueak RSqueakGit
curl -L -O http://www.libsdl.org/release/SDL-1.2.15.dmg
hdiutil mount SDL-1.2.15.dmg
ls /Volumes/*SDL*/
sudo cp -R /Volumes/*SDL*/SDL.framework /Library/Frameworks/

# env
export PY32="arch -32 /System/Library/Frameworks/Python.framework/Versions/2.7/bin/python"
export PYTHONPATH="$PYTHONPATH:pypy/:rsdl/:."

# script
$PY32 pypy/rpython/bin/rpython --batch -Ojit targetrsqueak.py
exitcode=$?

# after_success
if [ $exitcode -eq 0 ]; then
    cd RSqueakGit
    export RSQUEAKCOMMIT="$(git log --pretty=%h HEAD^..HEAD)"
    cd ..
    cp rsqueak rsqueak-x86-darwin-jit-$RSQUEAKCOMMIT
    curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/
fi
