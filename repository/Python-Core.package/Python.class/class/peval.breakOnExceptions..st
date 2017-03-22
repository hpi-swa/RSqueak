execution
peval: aString breakOnExceptions: aBool
	^ self
		primEval: aString
		filename: (self persistEvalCode: aString)
		cmd: 'eval'
		breakOnExceptions: aBool