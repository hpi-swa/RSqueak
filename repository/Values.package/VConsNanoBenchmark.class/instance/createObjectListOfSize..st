benchmark hooks
createObjectListOfSize: aNumber

	| rng |
	rng := Random seed: 36rSQUEAKSMALLTALK.
	^ self consWith: [SMarkBox new value: (rng nextInt: aNumber)] through: aNumber