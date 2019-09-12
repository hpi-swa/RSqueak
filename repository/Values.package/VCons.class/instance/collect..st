collection api
collect: aBlock

	| cons acc |
	cons := self.
	acc := VNil nil.
	[cons isEmpty] whileFalse:
		[cons isCons ifFalse: [^ self error: 'Not a proper list'].
		acc := self class car: (aBlock value: cons car) cdr: acc.
		cons := cons cdr].
	^ acc reversed