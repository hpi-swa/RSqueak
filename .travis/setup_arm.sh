#!/bin/bash
# Based on a test script from avsm/ocaml repo https://github.com/avsm/ocaml
set -ex
sudo apt-get update

CHROOT_DIR=$PWD/raspbian_arm
MIRROR=http://archive.raspbian.org/raspbian
VERSION=wheezy
CHROOT_ARCH=armhf

# Debian package dependencies for the host
HOST_DEPENDENCIES="debootstrap qemu-user-static binfmt-support sbuild scratchbox2 gcc-arm-linux-gnueabihf libsdl1.2-dev libffi-dev"

# Debian package dependencies for the chrooted environment
GUEST_DEPENDENCIES="build-essential git m4 sudo python libffi-dev libsdl1.2-dev"

# Command used to run the tests
TEST_COMMAND="python .build/build.py"

function setup_arm_chroot {
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

    # Create build dir and copy travis build files to our chroot environment
    sudo mkdir -p ${CHROOT_DIR}/${TRAVIS_BUILD_DIR}
    sudo rsync -av ${TRAVIS_BUILD_DIR}/ ${CHROOT_DIR}/${TRAVIS_BUILD_DIR}/

    # Indicate chroot environment has been set up
    sudo touch ${CHROOT_DIR}/.chroot_is_done

    # Call ourselves again which will cause tests to run
    sudo chroot ${CHROOT_DIR} bash -c "cd ${TRAVIS_BUILD_DIR} && ./.travis-ci.sh"
}

# ARM test run, need to set up chrooted environment first
echo "Setting up chrooted ARM environment"
setup_arm_chroot

if [ -e "/.chroot_is_done" ]; then
  # We are inside ARM chroot
  echo "Chrooted environment ready"
fi

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

schroot -c $CHROOT_DIR -- uname -m

pushd $CHROOT_DIR
sb2-init -c `which qemu-arm` ARM `which arm-linux-gnueabihf-gcc`
popd
