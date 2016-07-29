random-sampling
normalizeSkills

	"Normalizes the average skill to match Elo scores"
	| mean | 
	mean := self playerSkills values average.
	self playerSkills: (self playerSkills collect: [ :skill |
		skill / mean * (self targetSkillMean)])
	