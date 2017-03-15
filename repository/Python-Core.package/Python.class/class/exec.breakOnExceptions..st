execution
exec: aString breakOnExceptions: aBool
	^ self
		primEval: aString
		filename: '<string>'
		cmd: 'exec'
		breakOnExceptions: aBool