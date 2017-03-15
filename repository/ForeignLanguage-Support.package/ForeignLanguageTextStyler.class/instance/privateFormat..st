overrides
privateFormat: aText
	Smalltalk at: self languageSymbol ifPresent: [ :cls |
		(cls respondsTo: #stylerFormat:)
			ifTrue: [ ^ cls stylerFormat: aText ] ].
	^ super privateFormat: aText