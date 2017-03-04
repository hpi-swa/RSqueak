#!/bin/bash
set -ex

readonly BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

export OPTIONS=""

setup_osx() {
    case "${BUILD_ARCH}" in
      64bit)
      # brew update

      # Use PyPy2 v5.4.0
      brew install https://raw.githubusercontent.com/Homebrew/homebrew-core/c5a201f49c9da47d4771ebc544d10b3f9c579021/Formula/pypy.rb

      brew install sdl2
      ;;
    esac
}

setup_linux() {
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
	    g++-multilib \
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
	    export OPTIONS="--32bit"
	    ;;
	64bit)
	    PACKAGES="
	    libfreetype6:i386 \
	    "
	    ;;
	arm*)
	    PACKAGES="
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
	    export OPTIONS="--32bit"
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

    if [[ "${BUILD_ARCH}" = arm* ]]; then
	"${BASE}/setup_arm.sh"
    fi
}

# Only build arm on master
if [[ "${TRAVIS_BRANCH}" != "master" ]] && [[ "${BUILD_ARCH}" = arm* ]]; then
    exit 0
fi

setup_$TRAVIS_OS_NAME
python .build/download_dependencies.py $OPTIONS

if [[ -d ".build/sqpyte" ]]; then
  # Make sqlite/sqpyte for DatabasePlugin
  pushd ".build/sqpyte" > /dev/null
  chmod +x ./sqlite/configure
  sudo make
  popd > /dev/null
fi
