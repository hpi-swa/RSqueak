overrides
guessTypeForName: aString 
	self selectedContext isForeign ifFalse: [ ^ super guessTypeForName: aString ].
	^ nil