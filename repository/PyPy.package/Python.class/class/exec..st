helpers
exec: aString
	| filename |
	filename := self persistPyCode: aString.
	[ ^ self primEval: aString filename: filename cmd: 'exec' ] on: Error
		do: [ self error: 'Failed to exec: ''', aString, '''' ]