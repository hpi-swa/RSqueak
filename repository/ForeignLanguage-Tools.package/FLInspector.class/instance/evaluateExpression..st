overrides
evaluateExpression: selection
	self isForeign ifFalse: [self error: 'Smalltalk objects should be opened in original Inspector'].
	^ self languageClass evaluateExpression: selection in: self doItContext to: self doItReceiver