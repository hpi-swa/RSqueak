set up
vector: aVector with: aBlock through: aNumber

	^ aNumber = 0
		ifTrue: [aVector]
		ifFalse: [self vector: (aVector copyWith: aBlock value) with: aBlock through: (aNumber - 1)]