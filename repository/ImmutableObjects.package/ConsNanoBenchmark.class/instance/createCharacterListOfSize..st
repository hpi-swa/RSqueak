as yet unclassified
createCharacterListOfSize: aNumber

	| rng |
	rng := Random seed: 36rSQUEAKSMALLTALK.
	self consWith: [($a to: $z) atRandom: rng] though: aNumber