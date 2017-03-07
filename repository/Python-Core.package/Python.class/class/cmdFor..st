helpers
cmdFor: aPySource
	(self isExpression: aPySource)
		ifTrue: [ ^ 'eval' ]
		ifFalse: [ ^ 'exec' ]