execution
run: pyCode
	(self isExpression: pyCode)
		ifTrue: [ [^ self eval: pyCode ] on: Error do: [] ].
	^ self exec: pyCode