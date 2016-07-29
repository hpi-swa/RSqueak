testing
testMovePoints
	| a b |
	a := EloPlayer new score: 100.
	b := EloPlayer new score: 100.
	
	a scorePoints: 5 from: b.
	self assert: (a score = 105).
	self assert: (b score = 95).