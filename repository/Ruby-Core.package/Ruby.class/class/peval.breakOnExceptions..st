execution
peval: aString breakOnExceptions: aBool
	^ self primEval: aString filePath: (self persistEvalCode: aString) breakOnExceptions: aBool