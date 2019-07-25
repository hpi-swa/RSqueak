benchmark hooks
createCharacterListOfSize: aNumber

	| rng |
	rng := Random seed: 36rSQUEAKSMALLTALK.
	^ self consWith: [($a to: $z) atRandom: rng] through: aNumber