benchmark hooks
createNumberListOfSize: aNumber

	| rng |
	rng := Random seed: 36rSQUEAKSMALLTALK.
	^ self vectorWith: [rng nextInt: aNumber] through: aNumber