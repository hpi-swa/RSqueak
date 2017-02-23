helpers
cmdFor: aPySource
	(self evaluable: aPySource)
		ifTrue: [ ^ 'eval' ]
		ifFalse: [ ^ 'exec' ]