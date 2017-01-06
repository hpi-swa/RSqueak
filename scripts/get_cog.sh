#!/bin/bash
set -e

if ( echo $0 | grep 64 ); then
    words=64
    pkg=cog_linux64x64_squeak.cog.spur
    product=sqcogspur64linuxht
elif [[ "$(uname -m)" =~ "arm" ]]; then
    words=32
    pkg=cog_linux32ARMv6_squeak.cog.spur
    product=sqcogspurlinuxhtRPi
elif [ "$(uname)" == "Darwin" ]; then
    words=32
    pkg=cog_macos32x86_squeak.cog.spur
    product=CocoaFast.app
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
    version=$(curl -sL "https://${credentials}@api.bintray.com/packages/opensmalltalk/vm/cog" | grep -oE latest_version\"\:\"[0-9]+\" | grep -oE "[0-9]+")
    echo "Downloading latest Cog VM: ${version}"
fi
COGURL="https://${credentials}@dl.bintray.com/opensmalltalk/vm/${pkg}_${version}.tar.gz"

curl -sLO "$COGURL"

for i in cogspurlinux*; do
    test -d $i && rm -rf $i
done
tar xzf ${pkg}_${version}.tar.gz
rm ${pkg}_${version}.tar.gz
mv products/${product} cog${words}_new || mv ${product} cog${words}_new
rm -rf cog${words}
rm -rf products
if [ "$(uname)" == "Darwin" ]; then
    mkdir cog${words}
    mv cog${words}_new cog${words}/Cog.app
    cat <<EOF>> cog${words}/squeak
#!/bin/bash
$(pwd)/cog${words}/Cog.app/Contents/MacOS/Squeak $@
EOF
    chmod +x cog${words}/squeak
else
    mv cog${words}_new cog${words}
fi
