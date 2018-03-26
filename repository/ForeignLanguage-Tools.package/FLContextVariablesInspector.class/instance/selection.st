overrides
selection
	self object isForeign ifFalse: [ ^ super selection ].
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object foreignFrame ].
	Smalltalk at: object languageSymbol ifPresent: [ :cls |
		selectionIndex = 2 ifTrue: [^ cls getFilename: object foreignFrame ].
		^ cls tempVariableAt: selectionIndex - 2 in: object foreignFrame ].
	^ 'Unexpected selection'