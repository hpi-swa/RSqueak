overrides
aboutToStyle: aStyler
	(super aboutToStyle: aStyler) ifFalse: [ ^ false ].
	aStyler languageSymbol: self languageSymbol.
	^ true