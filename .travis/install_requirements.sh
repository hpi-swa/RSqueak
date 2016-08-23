#!/bin/bash
set -ex

readonly BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

presetup_osx() {
    echo "OS X Pre-setup"
}

setup_osx() {
    brew update
    brew install openssl

    case "${BUILD_ARCH}" in
      64bit)
      brew install pypy
      ;;
    esac

    SDL_DMG=SDL2-2.0.3.dmg
    curl -L -O http://www.libsdl.org/release/${SDL_DMG}
    hdiutil mount ${SDL_DMG}
    ls /Volumes/*SDL*/
    sudo ditto /Volumes/*SDL*/SDL2.framework /Library/Frameworks/SDL2.framework
    # todo: Squeak for jittests

    # Don't install coveralls on OS X, because it's too slow (see #116)
    # curl -L -O https://bootstrap.pypa.io/get-pip.py
    # sudo python get-pip.py
    # sudo pip install coveralls pytest-cov
}

presetup_linux() {
    sudo dpkg --add-architecture i386
    sudo apt-add-repository multiverse
    sudo apt-add-repository universe
    sudo apt-get update -myq || true

    case "$BUILD_ARCH" in
	32bit|lldebug)
	    PACKAGES="
	    gcc-multilib \
	    libasound2-dev:i386 \
	    libbz2-1.0:i386 \
	    libc6-dev-i386 \
	    libc6:i386 \
	    libexpat1:i386 \
	    libffi-dev:i386 \
	    libffi6:i386 \
	    libfreetype6:i386 \
	    libgcrypt11:i386 \
	    libgl1-mesa-dev:i386 \
	    mesa-common-dev:i386 \
	    libgl1-mesa-dri:i386 \
	    libgl1-mesa-glx:i386 \
	    libglapi-mesa:i386 \
	    libglu1-mesa-dev:i386 \
	    libglu1-mesa:i386 \
	    libssl1.0.0:i386 \
      libssl-dev:i386 \
	    libstdc++6:i386 \
	    libtinfo5:i386 \
	    libxext-dev:i386 \
	    libxt-dev:i386 \
	    zlib1g:i386 \
	    "
	    ;;
	64bit)
	    PACKAGES="
	    libfreetype6:i386 \
	    libsdl2-dev \
	    "
	    ;;
	arm*)
	    PACKAGES="
	    libsdl2-dev \
	    gcc-arm-linux-gnueabi \
	    gcc-arm-linux-gnueabihf \
	    qemu-system \
	    qemu-system-arm \
	    qemu-user \
	    qemu-user-static \
	    sbuild \
	    schroot \
	    scratchbox2 \
	    debootstrap \
	    zlib1g:i386 \
	    libstdc++6:i386 \
	    libffi-dev:i386 \
	    libffi6:i386 \
	    libssl1.0.0:i386 \
      libssl-dev:i386 \
	    libbz2-1.0:i386 \
	    libc6-dev-i386 \
	    libc6:i386 \
	    libexpat1:i386 \
	    libtinfo5:i386 \
	    "
	    ;;
    esac

    sudo apt-get install -yq \
	 --no-install-suggests --no-install-recommends --force-yes \
	 binfmt-support \
	 build-essential \
	 python-dev \
	 libffi-dev \
	 zlib1g-dev \
	 $PACKAGES
}

setup_linux() {
    # on Linux, libsdl2 is installed through our dependencies stuff
    wget http://squeakvm.org/unix/release/Squeak-4.10.2.2614-linux_i386.tar.gz
    tar xzvf Squeak-4.10*.tar.gz
    rm Squeak-4.10*.tar.gz
    ln -s $PWD/Squeak-4.10*/bin/squeak .build/squeak
    case "${BUILD_ARCH}" in
    	32bit)
		    # also install coveralls
			export PATH=.build/pypy-linux32/bin/:$PATH
			pip install coveralls
			;;
    	arm*)
			"${BASE}/setup_arm.sh"
			;;
	esac
}

# Only build arm on master
if [[ "${TRAVIS_BRANCH}" != "master" ]] && [[ "${BUILD_ARCH}" = arm* ]]; then
    exit 0
fi

presetup_$TRAVIS_OS_NAME
python .build/download_dependencies.py || true
setup_$TRAVIS_OS_NAME

if [[ -d ".build/sqpyte" ]]; then
  # Make sqlite/sqpyte for DatabasePlugin
  pushd ".build/sqpyte" > /dev/null
  chmod +x ./sqlite/configure
  sudo make
  popd > /dev/null
fi
