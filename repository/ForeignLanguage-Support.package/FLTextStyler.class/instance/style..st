overrides
style: aText
	self isForeign ifFalse: [ ^ super style: aText ].
	^ self style: aText foreignClass: self languageClass