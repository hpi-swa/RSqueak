benchmark
runTimes: n

	^(1 to: n) collect: [ :each | self runOnce]