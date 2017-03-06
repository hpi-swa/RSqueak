functional iteration
reduce: aBlock

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	^ self cdr foldl: aBlock withInit: self car