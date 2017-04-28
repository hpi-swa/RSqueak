overrides
selection
	self object isForeign ifFalse: [ ^ super selection ].
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object ].
	selectionIndex = 2 ifTrue: [^ object foreignFrame ].
	Smalltalk at: object languageSymbol ifPresent: [ :cls |
		selectionIndex = 3 ifTrue: [^ cls getFilename: object foreignFrame ].
		^ cls tempVariableAt: selectionIndex - 3 in: object foreignFrame ].
	^ 'Unexpected selection'