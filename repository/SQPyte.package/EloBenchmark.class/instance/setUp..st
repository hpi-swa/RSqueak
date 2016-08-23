as yet unclassified
setUp: matches
	| gen |
	self
		numMatches: matches;
		numPlayers: 200;
		seed: 42.
	gen := EloDataGenerator new seed: (self seed).
	DBObject beginTransaction.
	data := gen generatePlayers: self numPlayers matches: self numMatches.
	DBObject endTransaction.