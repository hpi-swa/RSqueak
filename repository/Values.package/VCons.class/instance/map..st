functional iteration
map: aBlock

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	^ self class
		car: (aBlock value: self car)
		cdr: (self cdr map: aBlock)