#!/bin/bash
set -ex

#set EX to sudo if required.
EX=sudo


if [ "$BUILD_ARCH" == "armv6" -o "$BUILD_ARCH" == "armv7" ]; then
    $EX chown $USER /etc/schroot/schroot.conf
    echo "
[precise_arm]
directory=$PWD/precise_arm
users=$USER
root-users=$USER
groups=$USER
aliases=default
type=directory

[rpi]
directory=$SB2
users=$USER
root-users=$USER
groups=$USER
aliases=default
type=directory
" >>  /etc/schroot/schroot.conf
    cat /etc/schroot/schroot.conf
    $EX chown root /etc/schroot/schroot.conf
fi

if [ "$BUILD_ARCH" == "armv6" ]; then
    MIRROR=http://archive.raspbian.org/raspbian
    VERSION=wheezy
    git clone https://github.com/raspberrypi/tools.git tools
    TOOLS_DIR=$PWD/tools

    # chroot
    mkdir ${SB2}
    qemu-debootstrap --no-check-gpg --include=fakeroot,build-essential --arch=armhf ${VERSION} ${SB2} ${MIRROR}
    echo "deb ${MIRROR} wheezy main contrib rpi" > ${SB2}/etc/apt/sources.list
    schroot -c rpi -u root -- apt-get update
    schroot -c rpi -u root -- apt-get --allow-unauthenticated install -qq -y build-essential python libffi-dev libsdl1.2-dev libpulse-dev
    # sb2
    pushd $SB2
    sb2-init -c `which qemu-arm` $SB2NAME $TOOLS_DIR/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian/bin/arm-linux-gnueabihf-gcc
    sb2-config -d rasp
    popd
elif [ "$BUILD_ARCH" == "armv7" ]; then
    mkdir precise_arm
    qemu-debootstrap --variant=buildd --arch=armel precise precise_arm/ http://ports.ubuntu.com/ubuntu-ports/
    schroot -c precise_arm -- uname -m
    echo "deb http://ports.ubuntu.com/ubuntu-ports/ precise main universe restricted" > precise_arm/etc/apt/sources.list
    schroot -c precise_arm -u root -- apt-get update
    schroot -c precise_arm -u root -- apt-get install -qq -y libffi-dev python-dev build-essential libsdl1.2-dev
    pushd precise_arm/
    sb2-init -c `which qemu-arm` ARM `which arm-linux-gnueabi-gcc`
    popd
fi
