testing
testImmutableFrom

	| objectUnderTest |
	objectUnderTest := self classUnderTest immutableFrom: self objectUnderTest.
	self assertPropertiesOf: objectUnderTest.
