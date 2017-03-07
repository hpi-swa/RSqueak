helpers
pyDictKeys
	(self hasAttribute: '__dict__') ifTrue: [ ^ self __dict__ keys ].
	^ #()