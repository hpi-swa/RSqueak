execution
evaluateExpression: aSelection in: aContext to: receiver
	^ receiver instance_eval: aSelection asString