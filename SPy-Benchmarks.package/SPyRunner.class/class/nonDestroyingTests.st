benchmarks
nonDestroyingTests
	| suite |
	suite := KernelTests copy.
	suite 
		tests: (suite tests 
			reject: [ :eachTestCase | 
				"Those tests lead to VM-Assertion Errors, etc."
				#(testBenchFib testAllNamedFromTo testWaitTimeoutMSecs)
					includes: eachTestCase selector]).
	^ suite