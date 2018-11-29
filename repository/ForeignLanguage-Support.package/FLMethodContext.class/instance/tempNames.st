overrides
tempNames
	self isForeign ifFalse: [ ^ super tempNames ].
	^ self languageClass tempNamesIn: self foreignFrame