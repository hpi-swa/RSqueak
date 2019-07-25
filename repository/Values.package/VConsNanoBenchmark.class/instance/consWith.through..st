set up
consWith: aBlock through: aNumber

	^ aNumber = 0
		ifTrue: [VNil nil]
		ifFalse: [self classUnderBenching car: (aBlock cull: aNumber) cdr: (self consWith: aBlock through: (aNumber - 1))]