overrides
guessTypeForName: aString 
	self hasPyContextSelected ifFalse: [ ^ super guessTypeForName: aString ].
	^ nil