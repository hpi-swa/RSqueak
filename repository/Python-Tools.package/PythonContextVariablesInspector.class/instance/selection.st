overrides
selection
	self object isPython ifFalse: [ ^ super selection ].
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object ].
	selectionIndex = 2 ifTrue: [^ object symbolic].
	selectionIndex = 3 ifTrue: [^ object headerDescription].
	^ object pyFrame f_locals values at: selectionIndex - 3