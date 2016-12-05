functional iteration
foldr: aBlock withInit: anObject

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	^ aBlock
		value: (self cdr foldr: aBlock withInit: anObject)
		value: self car