execution
evaluateExpression: selection
	self isForeign ifFalse: [ ^ Compiler evaluate: selection asString ].
	Smalltalk at: self languageSymbol ifPresent: [ :cls |
		(cls respondsTo: #evaluateExpression:)
			ifTrue: [ ^ cls evaluateExpression: selection ] ]