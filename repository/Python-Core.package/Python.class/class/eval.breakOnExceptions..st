execution
eval: aString breakOnExceptions: aBool
	^ self
		primEval: aString
		filename: '<string>'
		cmd: 'eval'
		breakOnExceptions: aBool