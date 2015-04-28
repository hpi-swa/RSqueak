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
generate_chroot

# sb2
pushd $CHROOT_DIR
sb2-init -c `which qemu-arm` $SB2NAME $TOOLS_DIR/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian/bin/arm-linux-gnueabihf-gcc
sb2-config -d rasp
popd

# cd $CHROOT_DIR
# sb2-init -c `which qemu-arm` Raspi `which arm-linux-gnueabihf-gcc`

# sudo chown -R $USER:$USER $CHROOT_DIR
# pushd $CHROOT_DIR
# export CFLAGS="-march=armv6 -mfpu=vfp -mfloat-abi=hard -marm"
# sb2-init rasp $TOOLS_DIR/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian/bin/arm-linux-gnueabihf-gcc
# sb2-config -d rasp


# this was used to generate the zipped chroot environment
# function download_chroot() {
#     wget http://downloads.raspberrypi.org/raspbian_latest -O raspbian_latest.zip
#     unzip raspbian_latest.zip
#     START=$(sudo fdisk -l *raspbian-wheezy.img | grep img2 | awk '{print $2}')
#     mkdir /tmp/rasp
#     sudo mount *raspbian-wheezy.img /tmp/rasp/ -o offset=$((START*512))
#     sudo cp -ar /tmp/rasp $CHROOT_DIR
#     sudo umount /tmp/rasp
#     sudo rmdir /tmp/rasp
# }

function generate_chroot() {
    sudo qemu-debootstrap --no-check-gpg --include=fakeroot,build-essential --arch=${CHROOT_ARCH} ${VERSION} ${CHROOT_DIR} ${MIRROR}
    echo "deb ${MIRROR} wheezy main contrib rpi" > rasp.apt.list
    sudo mv rasp.apt.list ${CHROOT_DIR}/etc/apt/sources.list
    sudo chroot ${CHROOT_DIR} apt-get update
    sudo chroot ${CHROOT_DIR} apt-get --allow-unauthenticated install -qq -y ${GUEST_DEPENDENCIES}
}
