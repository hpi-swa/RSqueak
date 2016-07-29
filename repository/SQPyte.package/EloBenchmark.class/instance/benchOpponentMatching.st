benchmark
benchOpponentMatching
	data players collect: [ :player | 
		data findOpponentFor: player goodEnough: 0.0001]
	"most scores are very close, 
	keep this value low to run through a few items before returning"