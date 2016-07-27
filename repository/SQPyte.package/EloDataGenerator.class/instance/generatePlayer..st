instance-generation
generatePlayer: playerId
	
	self playerSkills at: playerId put: (self sampleSkillOfPlayer: playerId).
	^EloPlayer new id: playerId.