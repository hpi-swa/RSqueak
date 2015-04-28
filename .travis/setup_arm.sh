#!/bin/bash
# Based on a test script from avsm/ocaml repo https://github.com/avsm/ocaml
set -ex
sudo apt-get update

CHROOT_NAME=raspbian_arm
CHROOT_DIR=$PWD/$CHROOT_NAME
MIRROR=http://archive.raspbian.org/raspbian
VERSION=wheezy
CHROOT_ARCH=armhf

# Debian package dependencies for the host
HOST_DEPENDENCIES="debootstrap qemu-user-static binfmt-support sbuild scratchbox2 gcc-arm-linux-gnueabihf libsdl1.2-dev libffi-dev"

# Debian package dependencies for the chrooted environment
GUEST_DEPENDENCIES="build-essential sudo python libffi-dev libsdl1.2-dev"

# Host dependencies
sudo apt-get install -qq -y ${HOST_DEPENDENCIES}

# Create chrooted environment
sudo mkdir ${CHROOT_DIR}
sudo debootstrap --foreign --no-check-gpg --include=fakeroot,build-essential \
    --arch=${CHROOT_ARCH} ${VERSION} ${CHROOT_DIR} ${MIRROR}
sudo cp /usr/bin/qemu-arm-static ${CHROOT_DIR}/usr/bin/
sudo chroot ${CHROOT_DIR} ./debootstrap/debootstrap --second-stage
sudo sbuild-createchroot --arch=${CHROOT_ARCH} --foreign --setup-only \
    ${VERSION} ${CHROOT_DIR} ${MIRROR}

# Install dependencies inside chroot
sudo chroot ${CHROOT_DIR} apt-get update
sudo chroot ${CHROOT_DIR} apt-get --allow-unauthenticated install \
    -qq -y ${GUEST_DEPENDENCIES}

sudo chown $USER /etc/schroot/schroot.conf
echo "
[raspbian_arm]
directory=$CHROOT_DIR
users=$USER
root-users=$USER
groups=$USER
aliases=default
type=directory
" >>  /etc/schroot/schroot.conf
cat /etc/schroot/schroot.conf
sudo chown root /etc/schroot/schroot.conf

schroot -c $CHROOT_NAME -- uname -m

cd $CHROOT_DIR
sb2-init -c `which qemu-arm` ARM `which arm-linux-gnueabihf-gcc`
