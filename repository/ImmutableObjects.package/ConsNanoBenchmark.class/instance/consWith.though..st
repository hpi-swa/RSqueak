as yet unclassified
consWith: aBlock though: aNumber

	^ aNumber = 0
		ifTrue: [nil]
		ifFalse: [Cons car: (aBlock cull: aNumber) cdr: (self consWith: aBlock though: (aNumber - 1))]