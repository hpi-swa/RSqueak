#!/bin/bash
set -ex


setup_osx() {
    SDL_DMG=SDL2-2.0.4.dmg
    curl -L -O http://www.libsdl.org/release/${SDL_DMG}
    hdiutil mount ${SDL_DMG}
    ls /Volumes/*SDL*/
    sudo ditto /Volumes/*SDL*/SDL2.framework /Library/Frameworks/SDL2.framework
    # todo: Squeak for jittests
}

setup_linux() {
    # borrowed this from https://github.com/omf2097/openomf/blob/master/.travis.yml
    # libsdl2-dev is not available yet on Ubuntu 12.04, which is employed by Travis
    if [ ! -d "$HOME/SDL2/lib" ]; then
        wget https://www.libsdl.org/release/SDL2-2.0.3.tar.gz -O ~/SDL2.tar.gz
        tar -xzvf ~/SDL2.tar.gz -C ~/
        mkdir ~/sdl-build
        pushd ~/sdl-build
        CONFIG_FLAGS=
        if [ "$BUILD_ARCH" == "32bit" -o "$BUILD_ARCH" == "lldebug" ]; then
            CONFIG_FLAGS="--build=i686-pc-linux-gnu \
                CFLAGS=-m32 CXXFLAGS=-m32 LDFLAGS=-m32"
        fi
        ~/SDL2-2.0.3/configure --prefix=$HOME/SDL2 $CONFIG_FLAGS
        make
        make install
        popd
    else
        echo 'Using cached SDL2 build directory.'
    fi
    wget http://squeakvm.org/unix/release/Squeak-4.10.2.2614-linux_i386.tar.gz
    tar xzvf Squeak-4.10*.tar.gz
    rm Squeak-4.10*.tar.gz
    ln -s $PWD/Squeak-4.10*/bin/squeak .build/squeak

}

setup_$TRAVIS_OS_NAME

python .build/download_dependencies.py

