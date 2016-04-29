#!/bin/bash
SQUEAK_SERVER="http://ftp.squeak.org/"
SOURCE_FOLDER="sources_files"
SQUEAK_WILDCARD="5.\d"
folder=$(curl -s $SQUEAK_SERVER | grep "/</a>" | grep -o "href=\".*\"" | grep -P "$SQUEAK_WILDCARD" | tail -1)
folder=${folder#*=}
folder=${folder#\"}
folder=${folder%%\"}
zip=$(curl -s $SQUEAK_SERVER/$folder | grep -o "href=\"Squeak.*zip\"")
zip=${zip#*=}
zip=${zip#\"}
zip=${zip%%\"}
curl -O "$SQUEAK_SERVER/${folder}${zip}"
unzip -o -f ${zip}
rm ${zip}
# Pull Squeak out of subdirectory
if [[ -z $(ls | grep Squeak.*image) ]]; then
    possible_squeak_dir="$(ls --group-directories-first | grep -m1 Squeak)"
    if [[ -d $possible_squeak_dir ]]; then
	mv "$possible_squeak_dir"/* .
	rmdir "$possible_squeak_dir"
    fi
fi
src=$(curl -s $SQUEAK_SERVER/$SOURCE_FOLDER/ | grep -o "href=\".*gz\"" | tail -1)
src=${src#*=}
src=${src#\"}
src=${src%%\"}
curl -O "$SQUEAK_SERVER/$SOURCE_FOLDER/${src}"
gunzip -f ${src}

exec $(dirname $0)/update_image.sh
