accessing
breakOnExceptionsText
	^ (self breakOnExceptions
		ifTrue: [ '<yes>' ]
		ifFalse: ['<no>' ]), ' break on exceptions'