execution
pexec: aString
	^ self
		primEval: aString
		filename: (self persistEvalCode: aString)
		cmd: 'exec'
		breakOnExceptions: false