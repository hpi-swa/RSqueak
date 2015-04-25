#!/bin/bash

python .build/download_dependencies.py

sudo apt-get update
sudo apt-get install -y build-essential libsdl1.2-dev libffi-dev

case "$BUILD_ARCH" in
    32bit)
	.travis/setup_chroot.sh
	;;
    64bit)
	;;
    arm)
	.travis/setup_arm.sh
	;;
esac
