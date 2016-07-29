testing
testSymmetricUpdate
	
	| black white match |
	black := EloPlayer new score: 100.
	white := EloPlayer new score: 100.
	match := (EloMatch new) blackPlayer: black; whitePlayer: white; outcome: 1.
	match adjustPlayerScores.
	self assert: black score > white score.