#!/bin/bash
set -ex

if [ "$TRAVIS_OS_NAME" == "osx" ]; then
    curl -L -O http://www.libsdl.org/release/SDL-1.2.15.dmg
    hdiutil mount SDL-1.2.15.dmg
    ls /Volumes/*SDL*/
    sudo cp -R /Volumes/*SDL*/SDL.framework /Library/Frameworks/
fi

python .build/download_dependencies.py

wget http://squeakvm.org/unix/release/Squeak-4.10.2.2614-linux_i386.tar.gz
tar xzvf Squeak-4.10*.tar.gz
rm Squeak-4.10*.tar.gz
ln -s $PWD/Squeak-4.10*/bin/squeak .build/squeak
