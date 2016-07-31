testing
testWinningProbability
	| a b c insane |
	a := EloPlayer new score: 110.
	b := EloPlayer new score: 100.
	c := EloPlayer new score: 90.
	insane := EloPlayer new score: 10000.
	
	self assert: (a chanceOfDefeating: b) between: 0.64 and: 0.65.
	self assert: (b chanceOfDefeating: c) between: 0.64 and: 0.65.
	self assert: (a chanceOfDefeating: c) between: 0.75 and: 0.76.
	self assert: (insane chanceOfDefeating: c) between: 0.9999 and: 1.0.