initialize-release
initialize
	self
		"amount of data generated"
		numPlayers: 1000;
		numMatches: 1000000;
		
		"distribution curve of player skills"
		minSkill: 20;
		maxSkill: 180;
		skillVariation: 30; 
		targetSkillMean: 100;
		"iff targetSkillMean is 100, then skill and Elo score are the same"
					
		"average amount a player deviates from its skill level (noise)"
		perMatchVariation: 5;
		perMatchMin: 0;  "smaller than minSkill"
		perMatchMax: 200;  "larger than maxSkill"
		
		"Local state"
		playerSkills: Dictionary new;
		random: Random new.