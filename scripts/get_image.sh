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
unzip ${zip}
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
gunzip ${src}

update_file="__squeak_update.st"
cat <<EOF> $update_file
TranscriptStream redirectToStdOut: true.
Utilities setAuthorInitials: 'benchmarkerCI'.
[[MCMcmUpdater updateFromServer]
	on: ProvideAnswerNotification
	do: [:n | n resume: true]]
	on: Warning
	do: [:n | n resume: true].
[(Installer squeak
	project: 'VMMaker')
	install: 'VMMaker.oscog'] on: Warning do: [:n | n resume: true].
(Installer swasource
	project: 'BenchmarkRunner')
	install: 'Benchmark';
	install: 'SMark'.
SystemWindow allSubInstancesDo: [:w | w delete].
(SmalltalkImage current snapshot: true andQuit: false) ifFalse: [
        SmalltalkImage current snapshot: false andQuit: true].
EOF
cogspurlinux/squeak Squeak*.image $update_file
rm $update_file
