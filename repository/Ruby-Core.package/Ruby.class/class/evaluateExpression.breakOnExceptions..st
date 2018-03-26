execution
evaluateExpression: selection breakOnExceptions: aBool
	^ Ruby eval: selection asString breakOnExceptions: aBool