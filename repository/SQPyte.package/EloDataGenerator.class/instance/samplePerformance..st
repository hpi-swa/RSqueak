random-sampling
samplePerformance: skill
	"Given an average skill, sample an instantaneous (noisy) performance of a player"
	
		^self
			sampleMean: skill
			deviation: self perMatchVariation
			min: self perMatchMin
			max: self perMatchMax.
	
	
	
	