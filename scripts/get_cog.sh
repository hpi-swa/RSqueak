#!/bin/bash
set -e

if ( echo $0 | grep 64 ); then
    words=64
    pkg=cog_linux64x64_squeak.cog.spur
    product=sqcogspur64linuxht
else
    words=32
    pkg=cog_linux32x86_squeak.cog.spur
    product=sqcogspurlinuxht
fi

credentials="$(cat `dirname $0`/bintray.credentials | head -1)"

if [ -n "$1" ]; then
    version=$1
    echo "Downloading version ${version}"
else
    version=$(curl -sL "https://${credentials}@api.bintray.com/packages/opensmalltalk/vm/cog" | grep -oP latest_version\"\:\"[0-9]+\" | grep -oP "[0-9]+")
    echo "Downloading latest Cog VM: ${version}"
fi
COGURL="https://${credentials}@dl.bintray.com/opensmalltalk/vm/${pkg}_${version}.tar.gz"

curl -sLO "$COGURL"

for i in cogspurlinux*; do
    test -d $i && rm -rf $i
done
rm -rf cog${words}
tar xzf ${pkg}_${version}.tar.gz
rm ${pkg}_${version}.tar.gz
mv products/${product} cog${words}
rm -rf products
