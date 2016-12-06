as yet unclassified
createNumberListOfSize: aNumber

	| rng |
	rng := Random seed: 36rSQUEAKSMALLTALK.
	self consWith: [rng nextInt: aNumber] though: aNumber