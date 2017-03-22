execution
pexec: aString breakOnExceptions: aBool
	^ self
		primEval: aString
		filename: (self persistEvalCode: aString)
		cmd: 'exec'
		breakOnExceptions: aBool