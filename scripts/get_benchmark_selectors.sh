#!/bin/bash
file="__run.st"
cat <<EOF> $file
Benchmark allSubclassesDo: [:class |
	class benchmarkSelectors do: [:sel |
		FileStream stdout nextPutAll: '"', class, '.', sel, '",'; cr]].
FileStream stdout flush.
EOF
cogspurlinux/squeak Squeak*.image $file
rm $file
