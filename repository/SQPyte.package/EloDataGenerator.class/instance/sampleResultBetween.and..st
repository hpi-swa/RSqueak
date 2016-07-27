random-sampling
sampleResultBetween: blackId and: whiteId

	"both players draw a number varying around their skill. The higher
	number wins."
	
	| blackPerf whitePerf |
	blackPerf := self samplePerformanceForId: blackId.
	whitePerf := self samplePerformanceForId: whiteId.
	^(blackPerf > whitePerf) ifTrue: 1 ifFalse: 0.
	
	
	