execution
run: pyCode breakOnExceptions: aBool
	(self isExpression: pyCode)
		ifTrue: [ [^ self eval: pyCode breakOnExceptions: aBool ] on: Error do: [] ].
	^ self exec: pyCode breakOnExceptions: aBool