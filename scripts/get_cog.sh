#!/bin/bash
COGURL="http://www.mirandabanda.org/files/Cog/VM/latest/"
zip=$(curl -s $COGURL | grep -o "href=\"cogspurlinux-.*tgz\"" | tail -1)
zip=${zip#*=}
zip=${zip#\"}
zip=${zip%%\"}
curl -O "$COGURL/$zip"
tar xzf $zip
rm $zip
