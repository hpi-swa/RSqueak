#!/bin/bash
set -ex

if [ "$BUILD_ARCH" == "armv6" ]; then
    CHROOT_DIR=$SB2
    MIRROR=http://archive.raspbian.org/raspbian
    VERSION=wheezy
    CHROOT_ARCH=armhf
    HOST_DEPENDENCIES="debootstrap qemu-user-static binfmt-support sbuild scratchbox2 gcc-arm-linux-gnueabihf libsdl1.2-dev libffi-dev libstdc++6:i386"
    GUEST_DEPENDENCIES="build-essential python libffi-dev libsdl1.2-dev libpulse-dev"

    # host deps
    sudo apt-get update
    git clone https://github.com/raspberrypi/tools.git tools
    TOOLS_DIR=$PWD/tools
    sudo apt-get install -qq -y ${HOST_DEPENDENCIES}

    # chroot
    sudo mkdir ${CHROOT_DIR}
    sudo qemu-debootstrap --no-check-gpg --include=fakeroot,build-essential --arch=${CHROOT_ARCH} ${VERSION} ${CHROOT_DIR} ${MIRROR}
    echo "deb ${MIRROR} wheezy main contrib rpi" > rasp.apt.list
    sudo mv rasp.apt.list ${CHROOT_DIR}/etc/apt/sources.list
    sudo chroot ${CHROOT_DIR} apt-get update
    sudo chroot ${CHROOT_DIR} apt-get --allow-unauthenticated install -qq -y ${GUEST_DEPENDENCIES}

    # sb2
    pushd $CHROOT_DIR
    sb2-init -c `which qemu-arm` $SB2NAME $TOOLS_DIR/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian/bin/arm-linux-gnueabihf-gcc
    sb2-config -d rasp
    popd
else if [ "$BUILD_ARCH" == "armv7" ]; then
    sudo apt-get update

    # install arm environment
    sudo apt-get install -y debootstrap schroot binfmt-support qemu-system qemu-user-static scratchbox2 gcc-arm-linux-gnueabi libsdl1.2-dev libffi-dev
    mkdir precise_arm
    sudo chown $USER /etc/schroot/schroot.conf
    echo "
[precise_arm]
directory=$PWD/precise_arm
users=$USER
root-users=$USER
groups=$USER
aliases=default
type=directory
" >>  /etc/schroot/schroot.conf
    cat /etc/schroot/schroot.conf
    sudo chown root /etc/schroot/schroot.conf

    sudo qemu-debootstrap --variant=buildd --arch=armel precise precise_arm/ http://ports.ubuntu.com/ubuntu-ports/
    schroot -c precise_arm -- uname -m
    sudo su -c 'echo "deb http://ports.ubuntu.com/ubuntu-ports/ precise main universe restricted" > precise_arm/etc/apt/sources.list'
    schroot -c precise_arm -u root -- apt-get update
    schroot -c precise_arm -u root -- apt-get install -y libffi-dev python-dev build-essential libsdl1.2-dev
    cd precise_arm/
    sb2-init -c `which qemu-arm` ARM `which arm-linux-gnueabi-gcc`
    cd ..
fi fi
