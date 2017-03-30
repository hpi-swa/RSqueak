execution
evaluateExpression: aSelection in: aContext to: receiver
	| pyCode |
	pyCode := aSelection asString.
	
	Python exec: 'rcvr_locals = dict()'.
	(Python eval: 'rcvr_locals') setdefault __call__: 'self' to: receiver.
	
	(Python isExpression: pyCode)
		ifTrue: [
			^ Python builtins eval
				__call__: pyCode
				globals: (Python eval: 'globals()')
				locals: (Python eval: 'rcvr_locals')]
		ifFalse: [
			^ Python builtins exec
				__call__: pyCode
				globals: (Python eval: 'globals()')
				locals: (Python eval: 'rcvr_locals')]