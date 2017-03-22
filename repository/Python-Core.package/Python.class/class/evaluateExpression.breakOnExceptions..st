execution
evaluateExpression: selection breakOnExceptions: aBool
	^ Python prun: selection asString breakOnExceptions: aBool
	