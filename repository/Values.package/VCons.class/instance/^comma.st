collection api
, aCons
	| cons acc |
	cons := self reversed. "reverse list to cons on aCons"
	acc := aCons.
	[cons isEmpty] whileFalse:
		[cons isCons ifFalse: [^ self error: 'Not a proper list'].
		acc := self class car: cons car cdr: acc.
		cons := cons cdr].
	^ acc