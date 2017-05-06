helpers
checkForException
	Smalltalk isHeadless ifTrue: [ ^ self ].
	(self primLastError) ifNotNil: [ :e |
		FLException new
			action: [ self openDebuggerOn: e ];
			signal ]