#!/bin/bash
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
