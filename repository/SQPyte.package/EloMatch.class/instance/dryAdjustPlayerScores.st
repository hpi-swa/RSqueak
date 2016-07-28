elo-ranking
dryAdjustPlayerScores
	| expected delta |
	expected := self blackPlayer chanceOfDefeating: (self whitePlayer).
	delta := (self outcome - expected) * EloMatch maxMovedPoints.
	"self blackPlayer scorePoints: delta from: self whitePlayer."
	