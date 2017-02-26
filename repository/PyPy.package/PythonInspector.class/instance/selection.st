as yet unclassified
selection
	"The receiver has a list of variables of its inspected object.
	One of these is selected. Answer the value of the selected variable."
	| attrName |
	selectionIndex = 0 ifTrue: [^ ''].
	selectionIndex = 1 ifTrue: [^ object].
	attrName := object allAttributes at: selectionIndex - 1.
	^ Python builtins getattr: object attrName: attrName.