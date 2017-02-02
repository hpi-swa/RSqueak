helpers
eval: aString
	| filename |
	filename := self persistPyCode: aString.
	[ ^ self primEval: aString filename: filename cmd: 'eval' ] on: Error
		do: [ self error: 'Failed to eval: ''', aString, '''' ]