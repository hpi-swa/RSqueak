overrides
styleInBackgroundProcess: aText
	self isForeign ifFalse: [ ^ super styleInBackgroundProcess: aText ].
	Smalltalk at: self languageSymbol ifPresent: [ :cls |
		(cls respondsTo: #styleInBackgroundProcess:)
			ifTrue: [ ^ cls styleInBackgroundProcess: aText ] ]