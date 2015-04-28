#!/bin/bash
set -ex

CHROOT_DIR=$SB2
MIRROR=http://archive.raspbian.org/raspbian
VERSION=wheezy
CHROOT_ARCH=armhf
HOST_DEPENDENCIES="debootstrap qemu-user-static binfmt-support sbuild scratchbox2 gcc-arm-linux-gnueabihf libsdl1.2-dev libffi-dev"
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
