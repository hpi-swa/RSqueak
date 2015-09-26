#!/bin/bash
set -ex

# Once we get the 64-bit build working, we'll have to split this up
sudo apt-get update

Packages="build-essential libffi-dev python-dev libffi6:i386 \
    libffi-dev:i386 libffi-dev libc6:i386 libc6-dev-i386 \
    libbz2-1.0:i386 libexpat1:i386 zlib1g:i386 libssl1.0.0:i386 \
    libgcrypt11:i386 libtinfo5:i386 gcc-multilib libfreetype6:i386"

if [ "$BUILD_ARCH" == "32bit" -o "$BUILD_ARCH" == "lldebug" ]; then
    # These are insane, i just want sdl1.2-dev:i386, but that multilib
    # package is broken
    Packages="$Packages libsdl1.2-dev:i386 libasound2-dev:i386 \
	libxt-dev:i386 libxext-dev:i386 \
	libglu1-mesa-dev:i386 libglu1-mesa:i386 \
	libgl1-mesa-dev:i386 libgl1-mesa-dri:i386 libgl1-mesa-glx:i386 \
	libglapi-mesa:i386"
    # FLTK compilation still fails
    # Packages="$Packages g++-multilib libfltk1.3-dev:i386"
fi

sudo apt-get install -y --force-yes $Packages

# borrowed this from https://github.com/omf2097/openomf/blob/master/.travis.yml
# libsdl2-dev is not available yet on Ubuntu 12.04, which is employed by Travis
if [ ! -d "$HOME/SDL2/lib" ]; then
    wget https://www.libsdl.org/release/SDL2-2.0.3.tar.gz -O ~/SDL2.tar.gz
    tar -xzvf ~/SDL2.tar.gz -C ~/
    mkdir ~/sdl-build
    cd ~/sdl-build
    ~/SDL2-2.0.3/configure --prefix=$HOME/SDL2
    make
    make install
else
    echo 'Using cached SDL2 build directory.'
fi

python .build/download_dependencies.py

wget http://squeakvm.org/unix/release/Squeak-4.10.2.2614-linux_i386.tar.gz
tar xzvf Squeak-4.10*.tar.gz
rm Squeak-4.10*.tar.gz
ln -s $PWD/Squeak-4.10*/bin/squeak .build/squeak

.travis/setup_arm.sh
