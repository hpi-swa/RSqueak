helpers
pyString
	(self hasAttribute: '__name__') ifTrue: [ ^ self __name__ ].
	self isClass ifFalse: [ ^ self __str__ ].
	^ 'unknown PyObject'