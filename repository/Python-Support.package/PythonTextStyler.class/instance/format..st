overrides
format: aText
	self isPythonCode
		ifTrue: [ ^ self formatPython: aText ]
		ifFalse: [ ^ super format: aText ]