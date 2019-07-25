benchmark hooks
createObjectListOfSize: aNumber

	| rng |
	rng := Random seed: 36rSQUEAKSMALLTALK.
	^ self vectorWith: [SMarkBox new value: (rng nextInt: aNumber)] through: aNumber