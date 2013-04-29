benchmarks
run
	"self run"
	| result printString |
	result := self runShootout.
	printString := self format: result.
	(Smalltalk vmParameterAt: 3) = 0
		ifTrue: [^printString] "SPy VM does not support stdout, yet"
		ifFalse: [FileStream stdout nextPutAll: printString]