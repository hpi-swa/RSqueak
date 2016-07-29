elo-ranking
chanceOfDefeating: anotherPlayer
	"An Elo ranking model to estimate the winning probability.
	A score difference of 10 results in a 64% chance, 20 in 76%"
	
	| scoreDiff |
	scoreDiff := anotherPlayer score - self score.
	^ 1.0 / (1.0 + (10.0 raisedTo: scoreDiff / 40.0))