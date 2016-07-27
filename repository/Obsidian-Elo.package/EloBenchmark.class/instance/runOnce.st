benchmark
runOnce
	| gen data |
	gen := EloDataGenerator new.
	data := gen generatePlayers: self numPlayers matches: self numMatches.
	^ [data adjustPlayerScores] timeToRun