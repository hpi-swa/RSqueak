overrides
validateMessageSource: sourceString forSelector: aSelector inClass: theClass
	"Check whether there is evidence that method source is invalid"
	self hasPyCodeSelected ifTrue: [^self].
	(theClass newParser parseSelector: sourceString asString) = aSelector
		ifFalse:  [self informPossiblyCorruptSource].