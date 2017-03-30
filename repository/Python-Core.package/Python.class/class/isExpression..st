helpers
isExpression: aPySource
	^ ((Python globals __getitem__ __call__: '_is_expression') __call__: aPySource) asSmalltalk