overrides
styleInBackgroundProcess: aText
	self isForeign ifFalse: [ ^ super styleInBackgroundProcess: aText ].
	^ Project current addDeferredUIMessage: [
		self style: aText foreignClass: self languageClass ]