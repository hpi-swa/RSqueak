benchmark hooks
createMixedListOfSize: aNumber

	| rng |
	rng := Random seed: 36rSQUEAKSMALLTALK.
	^ self 
		consWith: [
			| roll |
			roll := 3 atRandom: rng.
			roll = 1 ifTrue: [rng nextInt: aNumber] ifFalse: [
			roll = 2 ifTrue: [($a to: $z) atRandom: rng] ifFalse: [
			roll = 3 ifTrue: [SMarkBox new value: (rng nextInt: aNumber)]]]] 
		through: aNumber