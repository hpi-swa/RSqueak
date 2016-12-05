private mutation
replaceFrom: start to: stop with: replacement startingAt: repStart 
	" Use super with a private version of at:put:, as the actual one is not to be used"

	| index repOff |
	repOff := repStart - start.
	index := start - 1.
	[(index := index + 1) <= stop]
		whileTrue: [self pvtAt: index put: (replacement at: repOff + index)]