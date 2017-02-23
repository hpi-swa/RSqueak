helpers
run: pyCode
	(self evaluable: pyCode)
		ifTrue: [ [^ self eval: pyCode ] on: Error do: [] ].
	^ self exec: pyCode