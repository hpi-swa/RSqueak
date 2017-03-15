special methods
checkForException: aPyLanguage
	Smalltalk isHeadless ifTrue: [ ^ self ].
	(self primLastError: aPyLanguage) ifNotNil: [ :e |
		ForeignLanguageException new
			action: [ self openDebuggerWithPythonFrames: e ];
			signal ]