elo-ranking
scorePoints: points from: opponent
	"Moves points to me from an opponent.
	points can be positive for a win and negative for a loss."
	
	self score: (self score + points).
	opponent score: (opponent score - points)