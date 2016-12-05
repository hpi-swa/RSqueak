functional iteration
forEach: aBlock

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	aBlock value: self car.
	self cdr map: aBlock.