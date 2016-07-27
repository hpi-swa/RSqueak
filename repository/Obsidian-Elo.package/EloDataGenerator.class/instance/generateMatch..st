instance-generation
generateMatch: matchId

	"Draw two random opponents, simulate a game outcome according
	to their skills and return the match object"
	
	| black white outcome |
	black := self samplePlayerId.
	white := self samplePlayerExcept: black.
	outcome := self sampleResultBetween: black and: white.
	
	^(EloMatch new)
		id: matchId;
		blackPlayer: (self players at: black);
		whitePlayer: (self players at: white);
		outcome: outcome;
		yourself