overrides
selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	self isForeign ifFalse: [^ super selection].
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object].
	selectionIndex = 2 ifTrue: [^ 'tbd'].
	object isForeign ifTrue: [
		^ object instVarAt: selectionIndex - 2 ].
	^ super selection