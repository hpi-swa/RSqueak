functional iteration
append: aCons

	self cdr isCons ifFalse: [^ self error: 'Not a proper list'].
	^ VCons
		car: self car
		cdr: (self cdr append: aCons)