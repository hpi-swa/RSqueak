set up
consWith: aBlock through: aNumber

	^ aNumber = 0
		ifTrue: [nil]
		ifFalse: [Cons car: (aBlock cull: aNumber) cdr: (self consWith: aBlock through: (aNumber - 1))]