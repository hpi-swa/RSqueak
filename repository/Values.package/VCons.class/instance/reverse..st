functional iteration
reverse: acc

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	^ self cdr reverse: (self class car: self car cdr: acc)