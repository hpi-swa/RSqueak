#!/bin/bash
set -ex

sudo chown $USER /etc/schroot/schroot.conf
echo "
[rpi]
directory=$SB2
users=$USER
root-users=$USER
groups=$USER
aliases=default
type=directory
" >>  /etc/schroot/schroot.conf
cat /etc/schroot/schroot.conf
sudo chown root /etc/schroot/schroot.conf

MIRROR=http://archive.raspbian.org/raspbian
VERSION=jessie
git clone https://github.com/raspberrypi/tools.git tools
TOOLS_DIR=$PWD/tools

# chroot
mkdir ${SB2}
sudo qemu-debootstrap --no-check-gpg --include=fakeroot,build-essential --arch=armhf ${VERSION} ${SB2} ${MIRROR}
schroot -c rpi -- uname -m
sudo su -c "echo \"deb ${MIRROR} jessie main contrib rpi\" > ${SB2}/etc/apt/sources.list"
schroot -c rpi -u root -- apt-get update
schroot -c rpi -u root -- apt-get --allow-unauthenticated install -qq -y build-essential python libffi-dev libsdl2-dev libpulse-dev libssl-dev
schroot -c rpi -- python .build/download_dependencies.py
# sb2
pushd $SB2
sb2-init -c `which qemu-arm` $SB2NAME $TOOLS_DIR/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian/bin/arm-linux-gnueabihf-gcc
sb2-config -d rasp
popd
