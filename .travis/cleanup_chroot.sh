#!/bin/bash
set -ex

if [ "$BUILD_ARCH" == 32bit ]; then
    sudo umount "$PWD" "$chroot$PWD"
    sudo rm -rf "$chroot"/tmp/*
else
    sudo mkdir -p "$chroot"
fi
