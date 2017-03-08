overrides
style: aText
	self isPythonCode
		ifTrue: [ self stylePython: aText ]
		ifFalse: [ super style: aText ]