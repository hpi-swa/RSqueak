collection api
pairsCollect: aBlock

	| cons acc |
	cons := self.
	acc := VNil nil.
	[cons isEmpty] whileFalse:
		[ | consCdr |
		cons isCons ifFalse: [^ self error: 'Not a proper list'].
		(consCdr := cons cdr) isEmpty
			ifTrue: [cons := cons cdr]
			ifFalse: 
				[ | val |
				val := aBlock value: cons car value: consCdr car.
				acc := self class car: val cdr: acc.
				cons := consCdr cdr]].
	^ acc reversed