#!/bin/bash
set -e

git clone --depth=1 https://github.com/HPI-SWA-Lab/RSqueak.git
mv RSqueak/* .
mv RSqueak/.build .
mv RSqueak RSqueakGit

curl -L -O http://www.libsdl.org/release/SDL-1.2.15.dmg
hdiutil mount SDL-1.2.15.dmg
ls /Volumes/*SDL*/
sudo cp -R /Volumes/*SDL*/SDL.framework /Library/Frameworks/

# install
python .build/download_dependencies.py

# use 32bit python
export VERSIONER_PYTHON_PREFER_32_BIT=yes

# script
python .build/build.py
exitcode=$?

# after_success
if [ $exitcode -eq 0 ]; then
    cd RSqueakGit
    export RSQUEAKCOMMIT="$(git show -s --pretty=format:%H)"
    cd ..
    cp rsqueak rsqueak-x86-darwin-jit-$RSQUEAKCOMMIT
    curl -T rsqueak-x86* http://www.lively-kernel.org/babelsberg/RSqueak/
    curl -T rsqueak-x86* -utimfel:$BINTRAY_KEY https://api.bintray.com/content/hpi-swa-lab/RSqueak/Nightlies/$RSQUEAKCOMMIT/rsqueak-darwin-x86
    cp rsqueak rsqueak-darwin-latest
    curl -T rsqueak-darwin-latest http://www.lively-kernel.org/babelsberg/RSqueak/
    curl -T rsqueak-darwin-latest -utimfel:$BINTRAY_KEY https://api.bintray.com/content/hpi-swa-lab/RSqueak/Nightlies/latest/rsqueak-darwin-x86
    curl -X POST -utimfel:$BINTRAY_KEY https://api.bintray.com/content/hpi-swa-lab/RSqueak/Nightlies/latest/publish 
    curl -X POST -utimfel:$BINTRAY_KEY https://api.bintray.com/content/hpi-swa-lab/RSqueak/Nightlies/$RSQUEAKCOMMIT/publish 
fi
