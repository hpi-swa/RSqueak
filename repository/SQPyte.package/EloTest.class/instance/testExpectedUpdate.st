testing
testExpectedUpdate
	
	| black white match |
	black := EloPlayer new score: 110.
	white := EloPlayer new score: 90.
	match := (EloMatch new) blackPlayer: black; whitePlayer: white; outcome: 0.
	
	match adjustPlayerScores.
	self assert: (black score < 111) "black gains less points"