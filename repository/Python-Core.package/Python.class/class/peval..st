execution
peval: aString
	^ self
		primEval: aString
		filename: (self persistEvalCode: aString)
		cmd: 'eval'
		breakOnExceptions: false