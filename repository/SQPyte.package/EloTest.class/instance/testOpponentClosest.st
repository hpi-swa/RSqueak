as yet unclassified
testOpponentClosest
	
	| ranking player matched |
	player := EloPlayer new score: 100.
	ranking := EloRanking new players: {
		player.
		EloPlayer new score: 140.
		EloPlayer new score: 120.
		EloPlayer new score: 90}.
	matched := ranking findOpponentFor: player goodEnough: 0.
	self assert: [matched score = 90].
	matched := ranking findOpponentFor: player goodEnough: 30.
	self assert: [matched score = 120].
	