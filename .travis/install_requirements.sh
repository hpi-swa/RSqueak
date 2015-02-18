#!/bin/bash

# Download pypy and rsdl sources
wget https://bitbucket.org/pypy/pypy/get/default.tar.bz2 -O `pwd`/../pypy.tar.bz2 || wget https://bitbucket.org/pypy/pypy/get/default.tar.bz2 -O `pwd`/../pypy.tar.bz2
tar -xf `pwd`/../pypy.tar.bz2 -C `pwd`
mv pypy-pypy* pypy-pypy
wget https://bitbucket.org/pypy/rsdl/get/default.tar.bz2 -O `pwd`/../rsdl.tar.bz2 || wget https://bitbucket.org/pypy/rsdl/get/default.tar.bz2 -O `pwd`/../rsdl.tar.bz2
tar -xf `pwd`/../rsdl.tar.bz2 -C `pwd`
mv pypy-rsdl* pypy-rsdl

# This is actually only needed if "$BUILD_ARCH" == 64bit
sudo apt-get install -y build-essential libsdl1.2-dev libffi-dev python2.7
