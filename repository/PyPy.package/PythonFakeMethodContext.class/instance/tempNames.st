accessing
tempNames
	"Answer a SequenceableCollection of the names of the receiver's temporary 
	 variables, which are strings."

	self pyFrame ifNil: [^ #()].
	^ self pyFrame f_globals keys