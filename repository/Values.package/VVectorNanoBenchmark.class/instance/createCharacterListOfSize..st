benchmark hooks
createCharacterListOfSize: aNumber

	| rng |
	rng := Random seed: 36rSQUEAKSMALLTALK.
	^ self vectorWith: [($a to: $z) atRandom: rng] through: aNumber