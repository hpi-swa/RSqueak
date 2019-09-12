collection api
pairsDo: aBlock

	| cons |
	cons := self.
	[cons isEmpty] whileFalse: 
		[|consCdr |
		cons isCons ifFalse: [^ self error: 'Not a proper list'].
		(consCdr := cons cdr) isEmpty
			ifTrue: [cons := cons cdr]
			ifFalse: 
				[aBlock value: cons car value: consCdr car.
				cons := consCdr cdr]].
