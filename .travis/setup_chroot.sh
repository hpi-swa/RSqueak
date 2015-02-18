#!/bin/bash
set -ex

# Prepare an i386 chroot. This is required as we otherwise can't install
# our dependencies to be able to compile a 32bit binary. Ubuntu...
mkdir -p "$chroot$PWD"
sudo apt-get install -y debootstrap
sudo i386 debootstrap --arch=i386 precise "$chroot"
sudo mount --rbind "$PWD" "$chroot$PWD"
sudo i386 chroot "$chroot" apt-get install -y build-essential

# Now install our dependencies.
sudo i386 chroot "$chroot" apt-get install -y libsdl1.2-dev libffi-dev python2.7
