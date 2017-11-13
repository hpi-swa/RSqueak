execution
evaluateExpression: aSelection in: aContext to: receiver
	| rbCode rbBinding |
	rbCode := aSelection asString copyReplaceAll: 'self' with: 'that'.
	rbBinding := Ruby eval: 'blank_binding'.
	rbBinding local_variable_set: 'that' to: receiver.
	^ rbBinding eval: rbCode