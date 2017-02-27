#!/bin/bash
set -ex

readonly BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly TEST_IMAGES_BASE="${TRAVIS_BUILD_DIR}/rsqueakvm/test/images/"
readonly TEST_IMAGES_BASE_URL="https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/testing/images/"

presetup_osx() {
    echo "macOS Pre-setup"
}

setup_osx() {
    case "${BUILD_ARCH}" in
      64bit)
      # brew update

      # Use Pypy2 v5.4.0
      brew install https://raw.githubusercontent.com/Homebrew/homebrew-core/c5a201f49c9da47d4771ebc544d10b3f9c579021/Formula/pypy.rb
      
      brew install sdl2
      ;;
    esac

    if [[ "${PLUGINS}" = "PythonPlugin" ]]; then
      # Comment out SDL_messagebox.h in SDL.h
      # A wrong macro is applied to "const SDL_MessageBoxButtonData *buttons;"
      # which breaks compilation at the end.
      echo "Patching SDL.h for PythonPlugin"
      sed -i.bak "/SDL_messagebox\.h/s/^/\/\/ /" "/usr/local/include/SDL2/SDL.h"
    fi

    # todo: Squeak for jittests

    # Don't install coveralls on macOS, because it's too slow (see #116)
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

    if [[ "${PLUGINS}" = "PythonPlugin" ]]; then
      # Comment out SDL_messagebox.h in SDL.h
      # A wrong macro is applied to "const SDL_MessageBoxButtonData *buttons;"
      # which breaks compilation at the end.
      echo "Patching SDL.h for PythonPlugin"
      sudo sed -i.bak "/SDL_messagebox\.h/s/^/\/\/ /" "/usr/include/SDL2/SDL.h"
    fi
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

load_test_images() {
  local target
  local url

  if [[ -z "${TEST_TYPE}" ]]; then
    return
  fi

  if [[ "${PLUGINS}" = "PythonPlugin" ]]; then
    target="${TEST_IMAGES_BASE}/pypy.image"
    url="${TEST_IMAGES_BASE_URL}/pypy.image"
    curl -f -s -L --retry 3 -o "${target}" "${url}" 
  fi
}

# Only build arm on master
if [[ "${TRAVIS_BRANCH}" != "master" ]] && [[ "${BUILD_ARCH}" = arm* ]]; then
    exit 0
fi

presetup_$TRAVIS_OS_NAME
python .build/download_dependencies.py || true
setup_$TRAVIS_OS_NAME

load_test_images

if [[ -d ".build/sqpyte" ]]; then
  # Make sqlite/sqpyte for DatabasePlugin
  pushd ".build/sqpyte" > /dev/null
  chmod +x ./sqlite/configure
  sudo make
  popd > /dev/null
fi
