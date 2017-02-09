functional iteration
findf: aBlock ifNone: anotherBlock

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	^ (aBlock value: self car)
		ifTrue: [self car]
		ifFalse: [self cdr findf: aBlock ifNone: anotherBlock]