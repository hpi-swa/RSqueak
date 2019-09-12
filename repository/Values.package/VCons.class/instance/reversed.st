collection api
reversed

	| list cons |
	list := VNil nil.
	cons := self.
	[cons isEmpty] whileFalse: 
		[cons isCons ifFalse: [^ self error: 'Not a proper list'].
		list := self class car: cons car cdr: list.
		cons := cons cdr].
	^ list