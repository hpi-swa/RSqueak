#!/bin/bash
set -ex


setup_osx() {
    SDL_DMG=SDL2-2.0.3.dmg
    curl -L -O http://www.libsdl.org/release/${SDL_DMG}
    hdiutil mount ${SDL_DMG}
    ls /Volumes/*SDL*/
    sudo ditto /Volumes/*SDL*/SDL2.framework /Library/Frameworks/SDL2.framework
    # todo: Squeak for jittests
}

setup_linux() {
    # on Linux, libsdl2 is installed through our dependencies stuff
    wget http://squeakvm.org/unix/release/Squeak-4.10.2.2614-linux_i386.tar.gz
    tar xzvf Squeak-4.10*.tar.gz
    rm Squeak-4.10*.tar.gz
    ln -s $PWD/Squeak-4.10*/bin/squeak .build/squeak
}

setup_$TRAVIS_OS_NAME

python .build/download_dependencies.py

