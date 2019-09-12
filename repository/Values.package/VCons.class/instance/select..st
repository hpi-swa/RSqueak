collection api
select: aBlock

	| cons acc |
	cons := self.
	acc := VNil nil.
	[cons isEmpty] whileFalse: [
		cons isCons ifFalse: [^ self error: 'Not a proper list'].
		(aBlock value: cons car)
			ifTrue: [acc := self class car: cons car cdr: acc].
		cons := cons cdr].
	^ acc reversed