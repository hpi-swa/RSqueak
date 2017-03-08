overrides
styleInBackgroundProcess: aText
	self isPythonCode
		ifTrue: [ self stylePythonInBackgroundProcess: aText ]
		ifFalse: [ super styleInBackgroundProcess: aText ]