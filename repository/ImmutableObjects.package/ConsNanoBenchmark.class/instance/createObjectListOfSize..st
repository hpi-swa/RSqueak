as yet unclassified
createObjectListOfSize: aNumber

	| rng |
	rng := Random seed: 36rSQUEAKSMALLTALK.
	self consWith: [Box new value: (rng nextInt: aNumber)] though: aNumber