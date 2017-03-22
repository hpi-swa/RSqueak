helpers
cmdFor: pySource
	(self isExpression: pySource)
		ifTrue: [ ^ 'eval' ]
		ifFalse: [ ^ 'exec' ]