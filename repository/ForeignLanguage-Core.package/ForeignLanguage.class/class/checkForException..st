debugging
checkForException: languageProcess
	Smalltalk isHeadless ifTrue: [ ^ self ].
	(self primLastError: languageProcess) ifNotNil: [ :e |
		FLException new
			action: [ self openDebuggerOn: e languageProcess: languageProcess ];
			signal ]