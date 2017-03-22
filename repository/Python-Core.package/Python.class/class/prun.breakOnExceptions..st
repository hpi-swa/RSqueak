execution
prun: pyCode breakOnExceptions: aBool
	(self isExpression: pyCode)
		ifTrue: [ [^ self peval: pyCode breakOnExceptions: aBool ] on: Error do: [] ].
	^ self pexec: pyCode breakOnExceptions: aBool