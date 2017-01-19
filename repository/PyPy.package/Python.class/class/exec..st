helpers
exec: aString
	[ ^ self primEval: aString cmd: 'exec' ] on: Error
		do: [ self error: 'Failed to exec: ''', aString, '''' ]