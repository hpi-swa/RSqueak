elo-ranking
adjustPlayerScores
	"Estimate all player's Elo score given the match results"
	self matches do: [:match | match adjustPlayerScores]