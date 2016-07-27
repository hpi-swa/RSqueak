random-sampling
sampleMean: mu deviation: sigma min: min max: max
	"Draw random number centered around mu, deviating by sigma, clipped to an interval"
	
	| uniform scale result |
	
	scale := sigma * 0.5513288954217921. "3 sqrt / Float pi"
	result := min - 1.
	[result >= min and: [result <= max]] whileFalse: [
		uniform := self random next.
		"This is the logistic distribution"
		result := mu + ((uniform ln - ((1 - uniform) ln)) * scale)].
	^result
		 
	
	
	
	

	