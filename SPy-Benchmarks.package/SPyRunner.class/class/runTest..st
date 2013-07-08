benchmarks
runTest: aSymbol
	"self runTests"
	| suite |
	suite := KernelTests.
	suite := suite copy
		tests: (suite tests 
			select: [ :eachTestCase | 
				eachTestCase selector = aSymbol]).
	^ suite run asString