as yet unclassified
printString2
	Python vmSpeaksPython ifFalse: [ ^ super printString ].
	^ Python eval: self name asString, '.__class__.__name__'