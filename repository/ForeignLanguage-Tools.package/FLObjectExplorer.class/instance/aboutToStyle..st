as yet unclassified
aboutToStyle: aStyler
	(super aboutToStyle: aStyler) ifFalse: [ ^ false ].
	aStyler languageSymbol: self object languageSymbol.
	^ true