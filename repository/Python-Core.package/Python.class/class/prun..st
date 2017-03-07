execution
prun: pyCode
	(self isExpression: pyCode)
		ifTrue: [ [^ self peval: pyCode ] on: Error do: [] ].
	^ self pexec: pyCode