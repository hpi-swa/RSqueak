execution
evaluateExpression: aSelection in: aContext to: receiver
	| pyCode |
	pyCode := aSelection asString.
	
	Python exec: 'rcvr_locals = dict()'.
	(Python eval: 'rcvr_locals') setdefault: 'self' to: receiver.
	
	(Python isExpression: pyCode)
		ifTrue: [
			^ Python builtins
				eval: pyCode
				globals: (Python eval: 'globals()')
				locals: (Python eval: 'rcvr_locals')]
		ifFalse: [
			^ Python builtins
				exec: pyCode
				globals: (Python eval: 'globals()')
				locals: (Python eval: 'rcvr_locals')]

	"pyCode := pyCode copyReplaceAll: 'self.' with: ''.
	method := (pyCode copyUpTo: $() asSymbol.
	args := ((((pyCode copyAfter: $() copyUpTo: $))
		findTokens: {$,})
			collect: [ :ea | ea withBlanksTrimmed ]) asArray.
	^ receiver perform: method withArguments: args"