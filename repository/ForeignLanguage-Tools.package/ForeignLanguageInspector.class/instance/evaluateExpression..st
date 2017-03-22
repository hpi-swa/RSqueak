overrides
evaluateExpression: selection
	object languageSymbol = #Smalltalk ifTrue: [ ^ Compiler evaluate: selection asString ].
	^ self languageClass evaluateExpression: selection in: self doItContext to: self doItReceiver