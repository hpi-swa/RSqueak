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
	install: 'VMMaker.rsqueak'] on: Warning do: [:n | n resume: true].
(Installer swasource
	project: 'BenchmarkRunner')
	install: 'Benchmark';
	install: 'SMark'.
SystemWindow allSubInstancesDo: [:w | w delete].
(SmalltalkImage current snapshot: true andQuit: false) ifFalse: [
        SmalltalkImage current quitPrimitive].
EOF
timeout -s 9 1800 cog32/squeak Spur32.image $update_file
timeout -s 9 1800 cog64/squeak Spur64.image $update_file
# timeout -s 9 1800 squeakvm/bin/squeak V332.image $update_file
rm $update_file
