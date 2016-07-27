elo-ranking
adjustPlayerScoresWithAllInstances
	"Estimate all player's Elo score given the match results"
	EloMatch allInstancesDo: [:match | match adjustPlayerScores]