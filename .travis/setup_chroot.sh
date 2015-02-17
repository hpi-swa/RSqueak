#!/bin/bash
set -ex

# Download pypy and rsdl sources
wget https://bitbucket.org/pypy/pypy/get/default.tar.bz2 -O `pwd`/../pypy.tar.bz2 || wget https://bitbucket.org/pypy/pypy/get/default.tar.bz2 -O `pwd`/../pypy.tar.bz2
tar -xf `pwd`/../pypy.tar.bz2 -C `pwd`
mv pypy-pypy* pypy-pypy
wget https://bitbucket.org/pypy/rsdl/get/default.tar.bz2 -O `pwd`/../rsdl.tar.bz2 || wget https://bitbucket.org/pypy/rsdl/get/default.tar.bz2 -O `pwd`/../rsdl.tar.bz2
tar -xf `pwd`/../rsdl.tar.bz2 -C `pwd`
mv pypy-rsdl* pypy-rsdl

sudo apt-get install -y build-essential libsdl1.2-dev libffi-dev python2.7

if [ "$TEST_TYPE" == "64bitdefault" ]; then exit 0; fi
if [ "$TEST_TYPE" == "64bitbuild" ]; then exit 0; fi

# Prepare an i386 chroot. This is required as we otherwise can't install
# our dependencies to be able to compile a 32bit binary. Ubuntu...
mkdir -p "$chroot$PWD"
sudo apt-get install -y debootstrap
sudo i386 debootstrap --arch=i386 precise "$chroot"
sudo mount --rbind "$PWD" "$chroot$PWD"
sudo i386 chroot "$chroot" apt-get install -y build-essential

# Now install our dependencies.
sudo i386 chroot "$chroot" apt-get install -y libsdl1.2-dev libffi-dev python2.7
