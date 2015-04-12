#!/bin/bash

python .build/download_dependencies.py

# This is actually only needed if "$BUILD_ARCH" == 64bit
sudo apt-get install -y build-essential libsdl1.2-dev libffi-dev python2.7
