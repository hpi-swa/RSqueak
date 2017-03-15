execution
peval: aString
	^ self
		primEval: aString
		filename: (self persistPyCode: aString)
		cmd: 'eval'
		breakOnExceptions: self breakOnExceptions