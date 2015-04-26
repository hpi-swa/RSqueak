#!/bin/bash
set -ex

# Once we get the 64-bit build working, we'll have to split this up
sudo apt-get update
sudo apt-get install -y --force-yes build-essential \
    libffi-dev python-dev libffi6:i386 libffi-dev:i386 libffi-dev \
    libc6:i386 libc6-dev-i386 libbz2-1.0:i386 libexpat1:i386 zlib1g:i386 \
    libssl1.0.0:i386 libgcrypt11:i386 libtinfo5:i386 libsdl1.2-dev:i386 \
    gcc-multilib libfreetype6:i386 \
    libasound2-dev:i386 libxt-dev:i386 libxext-dev:i386 \
    libglu1-mesa-dev:i386  libglu1-mesa:i386 \
    libgl1-mesa-dev:i386 libgl1-mesa-dri:i386 libgl1-mesa-glx:i386 \
    libglapi-mesa:i386

python .build/download_dependencies.py

wget http://squeakvm.org/unix/release/Squeak-4.10.2.2614-linux_i386.tar.gz
tar xzvf Squeak-4.10*.tar.gz
rm Squeak-4.10*.tar.gz
ln -s $PWD/Squeak-4.10*/bin/squeak .build/squeak
