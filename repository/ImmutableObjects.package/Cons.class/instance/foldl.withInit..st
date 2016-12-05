functional iteration
foldl: aBlock withInit: anObject

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	^ self cdr foldl: aBlock withInit: (aBlock value: anObject value: self car)
