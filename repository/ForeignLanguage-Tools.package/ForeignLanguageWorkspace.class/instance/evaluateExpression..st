execution
evaluateExpression: selection
	self isForeign ifFalse: [ ^ Compiler evaluate: selection asString ].
	^ self languageClass evaluateExpression: selection breakOnExceptions: self breakOnExceptions