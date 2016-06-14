#!/bin/bash

# A 32-bit Spur-Trunk image
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
unzip -o ${zip}
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
mv Squeak*.image Spur32.image
mv Squeak*.changes Spur32.changes

# A 32-bit V3-Trunk image
curl -L -O http://build.squeak.org/job/FollowTrunkOnOldV3Image/lastSuccessfulBuild/artifact/trunkV3Images.zip
unzip -o trunkV3Images.zip
rm trunkV3Images.zip
mv Squeak*64.image V364.image
mvS queak*64.changes V364.changes
mv Squeak*.image V332.image
mv Squeak*.image V332.image

# A 64-bit Spur image
spurimgurl="http://www.mirandabanda.org/files/Cog/VM/SpurImages/"
image=$(curl -s $spurimgurl | grep -o "href=\".*\.image\"" | tail -1)
image=${image#*=}
image=${image#\"}
image=${image%%\"}
curl -L -O $spurimgurl$image
curl -L -O $spurimgurl${image%%.image}.changes
mv $image Spur64.image
mv ${image%%.image}.changes Spur64.changes


exec $(dirname $0)/update_image.sh
