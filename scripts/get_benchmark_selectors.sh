#!/bin/bash
file="__run.st"
cat <<EOF> $file
FileStream stdout nextPutAll: 'BENCHMARKS = ['; cr.
Benchmark allSubclassesDo: [:class |
    ((class category startsWith: 'Benchmark) or: [
      class isAbstract or: [
      class isAbstractClass]]) not ifTrue: [
        class benchmarkSelectors do: [:sel |
		FileStream stdout nextPutAll: '"', class, '.', sel, '",'; cr]]].
FileStream stdout nextPut: $]; cr; flush.
EOF
cogspurlinux/squeak Squeak*.image $file
rm $file
