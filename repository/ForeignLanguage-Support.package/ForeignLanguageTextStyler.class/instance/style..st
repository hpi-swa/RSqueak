overrides
style: aText
	self isForeign ifFalse: [ ^ super style: aText ].
	Smalltalk at: self languageSymbol ifPresent: [ :cls |
		(cls respondsTo: #highlight:)
			ifTrue: [ ^ self style: aText foreignClass: cls ] ]