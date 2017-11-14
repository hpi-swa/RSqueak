helpers
checkForException
	Smalltalk isHeadless ifTrue: [ ^ self ].
	(self primLastError) ifNotNil: [ :error |
		FLException new
			languageProcess: self;
			foreignError: error;
			signal ]