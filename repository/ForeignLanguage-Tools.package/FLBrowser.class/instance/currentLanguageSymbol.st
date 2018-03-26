overrides
currentLanguageSymbol
	self isPython ifTrue: [ ^ #Python ].
	self isRuby ifTrue: [ ^ #Ruby ].
	^ nil