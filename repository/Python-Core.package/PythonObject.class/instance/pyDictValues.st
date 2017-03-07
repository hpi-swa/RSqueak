helpers
pyDictValues
	(self hasAttribute: '__dict__') ifTrue: [ ^ self __dict__ values ].
	^ #()