overrides
tempNames
	"Answer a SequenceableCollection of the names of the receiver's temporary 
	 variables, which are strings."

	self foreignFrame ifNil: [^ #()].
	^ self pyFrame f_locals keys asSmalltalk