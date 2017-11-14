helpers
isExpression: aPySource
	^ [ self compile __call__: aPySource filename: '<string>' cmd: 'eval'. true ]
			on: FLException do: [:ex | (ex foreignError at: 0) asSmalltalk ~= 'SyntaxError' ]