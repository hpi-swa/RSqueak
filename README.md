## RSqueak/VM [![Linux Build Status][travis_badge]][travis] [![Windows Build Status][appveyor_badge]][appveyor] [![Coverage Status][coveralls_badge]][coveralls] [![Benchmarks][benchmarks_badge]][benchmarks] [![Documentation Status][docs_badge]][docs]

A Squeak VM written in RPython.

### Download

All-in-One bundle for Linux, Windows and macOS:

[![Download zip][dl_zip_badge]][dl_zip] [![Download tar.gz][dl_tgz_badge]][dl_tgz]

Pre-built 32-bit binaries:

[![Download Linux][dl_linux_badge]][dl_linux] [![Download macOS][dl_macos_badge]][dl_macos] [![Download Windows][dl_win_badge]][dl_win]

Pre-built 64-bit binaries (note that the goal for RSqueak/VM to support all
image formats that are <= the native word size - so the 64-bit VMs should open
both 32-bit and 64-bit images; it should simply be faster. Due to limitations of
the underlying RPython toolchain, Windows binaries cannot currently be built in
64-bit mode):

[![Download Linux x86_64][dl_linux64_badge]][dl_linux64] [![Download macOS x86_64][dl_macos64_badge]][dl_macos64]

We also have experimental builds for Raspberry Pi:

[![Download Raspberry Pi 1][dl_raspi1_badge]][dl_raspi1] [![Download Raspberry Pi 2][dl_raspi2_badge]][dl_raspi2] [![Download Raspberry Pi 3][dl_raspi3_badge]][dl_raspi3]

### Finding a working image

Although RSqueak can load images starting with Squeak 2, many primitives are not
implemented and instead rely on in-image fallback code to be available and
correct. For example, we do not have a BitBlt implementation, so unprepared
images will simply stay black. This is why only a Trunk image of Squeak with the
latest version of VMMaker from the VMMaker.oscog branch fully works. Try this in
a recent Trunk image to prepare it for use with RSqueak/VM:

```Smalltalk
(Installer squeak project: 'VMMaker') install: 'VMMaker.oscog'.
MCMcmUpdater updateFromServer.
```

Note that RSqueak/VM will *always* save images in 32-bit Cog-Spur format,
regardless of what it was when you opened it. So make sure to save as new
version if you want to keep the original around.

### [Building from Source][build_from_source]
### [Development][development]


[appveyor]: https://ci.appveyor.com/project/timfel/rsqueak
[appveyor_badge]: https://ci.appveyor.com/api/projects/status/e37a79tt5irr7sx1/branch/master?svg=true
[benchmarks]: http://speed.squeak.org/
[benchmarks_badge]: https://img.shields.io/badge/benchmarks-open-yellowgreen.svg
[build_from_source]: http://rsqueak.readthedocs.io/en/latest/building_from_source.html
[coveralls]: https://coveralls.io/github/hpi-swa/RSqueak?branch=master
[coveralls_badge]: https://coveralls.io/repos/github/hpi-swa/RSqueak/badge.svg?branch=master
[development]: http://rsqueak.readthedocs.io/en/latest/development.html
[dl_linux64]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-x86_64-latest
[dl_linux64_badge]: https://img.shields.io/badge/Download-Linux_x86__64-blue.svg
[dl_linux]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-latest
[dl_linux_badge]: https://img.shields.io/badge/Download-Linux-blue.svg
[dl_macos64]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-darwin-x86_64-latest
[dl_macos64_badge]: https://img.shields.io/badge/Download-Mac%20OS%20X%20x86__64-blue.svg
[dl_macos]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-darwin-latest
[dl_macos_badge]: https://img.shields.io/badge/Download-Mac_OS_X-blue.svg
[dl_raspi1]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-armv6raspbian-latest
[dl_raspi1_badge]: https://img.shields.io/badge/Download-Raspberry_Pi_1-blue.svg
[dl_raspi2]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-armv7-araspbian-latest
[dl_raspi2_badge]: https://img.shields.io/badge/Download-Raspberry_Pi_2-blue.svg
[dl_raspi3]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-linux-armv8-araspbian-latest
[dl_raspi3_badge]: https://img.shields.io/badge/Download-Raspberry_Pi_3-blue.svg
[dl_tgz]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/bundle/RSqueak.tar.gz
[dl_tgz_badge]: https://img.shields.io/badge/Download-tar.gz-blue.svg
[dl_win]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-win32-latest.exe
[dl_win_badge]: https://img.shields.io/badge/Download-Windows-blue.svg
[dl_zip]: https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/bundle/RSqueak.zip
[dl_zip_badge]: https://img.shields.io/badge/Download-zip-blue.svg
[docs]: http://rsqueak.readthedocs.io/en/latest/?badge=latest
[docs_badge]: https://readthedocs.org/projects/rsqueak/badge/?version=latest
[travis]: https://travis-ci.org/hpi-swa/RSqueak
[travis_badge]: https://travis-ci.org/hpi-swa/RSqueak.svg?branch=master
