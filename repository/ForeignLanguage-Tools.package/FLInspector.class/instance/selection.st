overrides
selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object].
	object isForeign ifTrue: [
		^ object instVarAt: selectionIndex - 1 ].
	^ super selection