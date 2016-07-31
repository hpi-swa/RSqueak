elo-ranking
dryAdjustPlayerScores
	"Estimate all player's Elo score given the match results"
	self matches do: [:match | match dryAdjustPlayerScores]