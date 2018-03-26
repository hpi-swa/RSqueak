overrides
languageSymbol
	^ self selectedContext isForeign
		ifTrue: [ self selectedContext languageSymbol ]
		ifFalse: [ #Smalltalk ]