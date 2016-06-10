#!/bin/bash

if [ -n "$1" ]; then
    echo "Downloading version ${1}"
    VERSION="$1"
else
    VERSION=""
fi
URL="http://squeakvm.org/unix/release/"

string="$(uname -m)"
if [[ "$string" == *"arm"* ]]; then
    zip=$(curl -s $URL | grep -o "href=\"Squeak-.*${VERSION}.*linux.*armv6.*.tar.gz\"" | tail -1)
else
    zip=$(curl -s $URL | grep -o "href=\"Squeak-.*${VERSION}.*linux.*i386.*.tar.gz\"" | tail -1)
fi

zip=${zip#*=}
zip=${zip#\"}
zip=${zip%%\"}
curl -O "$URL/$zip"
rm -rf squeakvm
tar xzf $zip
rm $zip
mv Squeak* squeakvm
