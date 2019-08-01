functional iteration
filter: aBlock

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	^ (aBlock value: self car)
		ifTrue: [self class car: self car cdr: (self cdr filter: aBlock)]
		ifFalse: [self cdr filter: aBlock]