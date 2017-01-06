#!/bin/bash
set -e

if ( echo $0 | grep 64 ); then
    words=64
    pkg=cog_linux64x64_squeak.sista.spur
    product=sqsistaspur64linuxht
elif [ "$(uname)" == "Darwin" ]; then
    words=32
    pkg=cog_macos32x86_squeak.sista.spur
    product=CocoaFast.app
else
    words=32
    pkg=cog_linux32x86_squeak.sista.spur
    product=sqsistaspurlinuxht
fi

credentials="$(cat `dirname $0`/bintray.credentials | head -1)"

if [ -n "$1" ]; then
    version=$1
    echo "Downloading version ${version}"
else
    version=$(curl -sL "https://${credentials}@api.bintray.com/packages/opensmalltalk/vm/cog" | grep -oP latest_version\"\:\"[0-9]+\" | grep -oP "[0-9]+")
    echo "Downloading latest Cog/Sista VM: ${version}"
fi
COGURL="https://${credentials}@dl.bintray.com/opensmalltalk/vm/${pkg}_${version}.tar.gz"

curl -sLO "$COGURL"

for i in sistaspurlinux*; do
    test -d $i && rm -rf $i
done
echo ${pkg}_${version}.tar.gz
tar xzf ${pkg}_${version}.tar.gz
rm ${pkg}_${version}.tar.gz
mv products/${product} sista${words}_new || mv ${product} sista${words}_new
rm -rf sista${words}
rm -rf products
if [ "$(uname)" == "Darwin" ]; then
    mkdir sista${words}
    mv sista${words}_new sista${words}/Sista.app
    ln -s $(pwd)/sista${words}/Sista.app/Contents/MacOS/Squeak sista${words}/squeak
else
    mv sista${words}_new sista${words}
fi
