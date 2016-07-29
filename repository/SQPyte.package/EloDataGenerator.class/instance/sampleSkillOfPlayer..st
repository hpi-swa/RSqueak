random-sampling
sampleSkillOfPlayer: playerId

	"A logistic distribution to have a large center mass and a few outliers"
	
	| mean |
	mean := (self maxSkill + self minSkill) / 2.0.
	^self
		sampleMean: mean
		deviation: self skillVariation
		min: self minSkill
		max: self maxSkill

	"A straight linear distribution, e.g. {20. 22. 24. ... 180}"
 	"^(self minSkill) + 		((self maxSkill - self minSkill) * (playerId - 1) / (self numPlayers - 1)) asFloat"