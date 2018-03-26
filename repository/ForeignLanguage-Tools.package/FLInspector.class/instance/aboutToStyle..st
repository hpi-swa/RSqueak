overrides
aboutToStyle: aStyler
	(super aboutToStyle: aStyler) ifFalse: [ ^ false ].
	aStyler languageSymbol: object languageSymbol.
	^ true