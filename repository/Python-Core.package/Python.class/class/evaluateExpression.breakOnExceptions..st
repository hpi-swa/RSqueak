execution
evaluateExpression: selection breakOnExceptions: aBool
	^ Python run: selection asString breakOnExceptions: aBool
	