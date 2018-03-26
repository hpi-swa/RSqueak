overrides
pcRange
	| ctx |
	ctx := self selectedContext.
	ctx isForeign ifFalse: [ ^ super pcRange ].
	Smalltalk at: ctx languageSymbol ifPresent: [ :cls |
		^ cls pcRange: ctx foreignFrame ].