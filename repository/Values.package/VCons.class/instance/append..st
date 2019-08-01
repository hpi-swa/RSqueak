functional iteration
append: aCons

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	^ self class
		car: self car
		cdr: (self cdr append: aCons)