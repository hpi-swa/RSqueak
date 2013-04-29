benchmarks
run
	"self run"
	| result result2 |
	result := self runShootout.
	result2 := self runTinyBenchmarks.
	result at: #benchmark put: (result2 at: #benchmark). 
	result at: #benchFib put: (result2 at: #benchFib). 
	
	^self format: result.
	