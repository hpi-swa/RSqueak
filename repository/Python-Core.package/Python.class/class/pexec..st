execution
pexec: aString
	^ self
		primEval: aString
		filename: (self persistPyCode: aString)
		cmd: 'exec'
		breakOnExceptions: self breakOnExceptions