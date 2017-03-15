special methods
checkForException: aRbLanguage
	Smalltalk isHeadless ifTrue: [ ^ self ].
	(self primLastError: aRbLanguage) ifNotNil: [ :e |
		1 halt ]