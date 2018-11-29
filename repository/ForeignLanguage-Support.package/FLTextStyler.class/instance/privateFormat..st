overrides
privateFormat: aText
	self isForeign ifFalse: [ ^ super privateFormat: aText ].
	^ self languageClass stylerFormat: aText