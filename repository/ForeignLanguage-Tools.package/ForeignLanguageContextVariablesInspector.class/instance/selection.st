overrides
selection
	self object isForeign ifFalse: [ ^ super selection ].
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object ].
	selectionIndex = 2 ifTrue: [^ object symbolic].
	selectionIndex = 3 ifTrue: [^ object headerDescription].
	Smalltalk at: object languageSymbol ifPresent: [ :cls |
		^ cls tempVariableAt: selectionIndex - 3 in: object foreignFrame ].
	^ 'Unexpected selection'