overrides
pcRange
	"Answer the indices in the source code for the method corresponding to 
	the selected context's program counter value."
	self hasPyContextSelected ifFalse: [ ^ super pcRange ].
	^ self pyPCRange