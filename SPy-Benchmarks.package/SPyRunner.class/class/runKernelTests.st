benchmarks
runKernelTests
	"self runTests"
	| result suite |
	suite := TestSuite named: 'RSqueakVM-Tests'.
	"To add later: MethodPragmaTest . WeakMessageSendTest"
	{IntegerTest . InstructionClientTest . FractionTest . DelayTest . CompiledMethodTest . BehaviorTest . StopwatchTest . YearTest . TimeTest . AllocationTest . ProcessTest . ClassDescriptionTest . SmallIntegerTest . MethodContextTest . CompiledMethodComparisonTest . YearMonthWeekTest . TimespanTest . DependentsArrayTest . CategorizerTest . IntegerDigitLogicTest . SemaphoreTest . PromiseTest . DateTest . DateAndTimeEpochTest . InstVarRefLocatorTest . DateAndTimeTest . BasicBehaviorClassMetaclassTest . ExtendedNumberParserTest . TrueTest . UndefinedObjectTest . ComplexTest . ScheduleTest . CompiledMethodTrailerTest . LargePositiveIntegerTest . ScaledDecimalTest . ClassBuilderTest . SqNumberParserTest . ProtoObjectTest . NumberParsingTest . RandomTest . DateAndTimeLeapTest . TimespanDoTest . ClassTest . TimespanDoSpanAYearTest . BlockContextTest . TimeStampTest . GradientFillStyleTest . MethodPropertiesTest . WeekTest . ObjectTest . DurationTest . NumberTest . MonthTest . FalseTest . InstructionPrinterTest . MonitorTest . BooleanTest . BlockClosureTest . FloatTest . ProcessSpecificTest . LargeNegativeIntegerTest} do: [ :each | each addToSuiteFromSelectors: suite].
	suite 
		tests: (suite tests 
			reject: [ :eachTestCase | 
				"Those tests lead to VM-Assertion Errors, etc."
				#(testBenchFib testMultiProcessWaitOnSameDelay testBehaviornewnewShouldNotCrash testAllNamedFromTo testChange testAtomicSuspend testWaitTimeoutMSecs testMonitorNotGainingUnwantedSignalsDuringUnwinding testDegreeCosForExceptionalValues testDegreeSinForExceptionalValues testInfinity1 testInfinity2)
					includes: eachTestCase selector]).
	result := suite run.
	^result asString