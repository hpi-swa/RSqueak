overrides
aboutToStyle: aStyler
	self isModeStyleable ifFalse: [^false].
	self selectedContext isForeign ifFalse: [^ super aboutToStyle: aStyler].
	aStyler
		classOrMetaClass: self selectedClassOrMetaClass;
		languageSymbol: self languageSymbol.
	^ true