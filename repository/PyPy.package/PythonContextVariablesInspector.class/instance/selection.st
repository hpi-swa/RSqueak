as yet unclassified
selection

	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object ].
	selectionIndex = 2 ifTrue: [^ object symbolic].
	selectionIndex = 3 ifTrue: [^ object headerDescription].
	^ object pyFrame f_globals values at: selectionIndex - 3