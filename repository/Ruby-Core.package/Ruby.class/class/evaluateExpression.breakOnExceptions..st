execution
evaluateExpression: selection breakOnExceptions: aBool
	^ Ruby peval: selection asString breakOnExceptions: aBool