execution
peval: aString
	^ self eval: aString filePath: (self persistEvalCode: aString)