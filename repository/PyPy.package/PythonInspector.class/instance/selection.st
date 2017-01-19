as yet unclassified
selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| attr |
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object].
	selectionIndex = 2 ifTrue: [^ object class].
	attr := object allAttributes at: selectionIndex - 2.
	^ object perform: attr asSymbol