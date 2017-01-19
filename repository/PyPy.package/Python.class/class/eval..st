helpers
eval: aString
	[ ^ self primEval: aString cmd: 'eval' ] on: Error
		do: [ self error: 'Failed to eval: ''', aString, '''' ]