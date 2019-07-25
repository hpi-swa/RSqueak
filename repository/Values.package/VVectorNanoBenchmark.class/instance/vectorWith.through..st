set up
vectorWith: aBlock through: aNumber

	^ aNumber isZero
		ifTrue: [self classUnderBenching new]
		ifFalse: [self classUnderBenching withAll: (self fillArrayOfSize: aNumber with: aBlock)]