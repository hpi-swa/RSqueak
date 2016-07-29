instance-generation
generatePlayers: numPlayers matches: numMatches

	self numPlayers: numPlayers.
	self numMatches: numMatches.
	self players: (self generatePlayers).
	self normalizeSkills.
	self matches: (self generateMatches).
	^(EloRanking new)
		matches: self matches;
		players: self players