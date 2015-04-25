#!/bin/bash
set -ex

sudo apt-get install -y debootstrap
if [ ! -d "$chroot" ]; then
    # Prepare an i386 chroot. This is required as we otherwise can't install
    # our dependencies to be able to compile a 32bit binary. Ubuntu...
    mkdir -p "$chroot$PWD"
    sudo i386 debootstrap --arch=i386 precise "$chroot"
    sudo i386 chroot "$chroot" apt-get install -y build-essential
    
    sudo chown $USER $chroot/etc/apt/sources.list
    echo "deb http://ppa.launchpad.net/pypy/ppa/ubuntu precise main" >> $chroot/etc/apt/sources.list    
    # echo "deb http://archive.ubuntu.com/ubuntu precise universe restricted multiverse" >> $chroot/etc/apt/sources.list
    cat $chroot/etc/apt/sources.list
    sudo chown root $chroot/etc/apt/sources.list

    # Now install our dependencies.
    sudo i386 chroot "$chroot" apt-get update
    sudo i386 chroot "$chroot" apt-get install -y --force-yes libsdl1.2-dev libffi-dev python-dev python2.7 pypy

    # I know, but the deb package pulls in dbus through dependencies,
    # and that won't work in 32bit chroot
    wget http://squeakvm.org/unix/release/Squeak-4.10.2.2614-linux_i386.tar.gz
    tar xzvf Squeak-4.10*.tar.gz
    rm Squeak-4.10*.tar.gz
    sudo rsync -avc Squeak-4.10*/ $chroot/usr/
    sudo i386 chroot "$chroot" apt-get install -y --force-yes libfreetype6
fi

sudo mount --rbind "$PWD" "$chroot$PWD"
