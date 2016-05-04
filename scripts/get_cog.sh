#!/bin/bash

if [ -n "$1" ]; then
    echo "Downloading version ${1}"
    COGURL="http://www.mirandabanda.org/files/Cog/VM/VM.r$1/"
else
    echo "Downloading latest Cog VM"
    COGURL="http://www.mirandabanda.org/files/Cog/VM/latest/"
fi

zip=$(curl -s $COGURL | grep -o "href=\"cogspurlinux-.*tgz\"" | tail -1)
zip=${zip#*=}
zip=${zip#\"}
zip=${zip%%\"}
curl -O "$COGURL/$zip"
tar xzf $zip
rm $zip
