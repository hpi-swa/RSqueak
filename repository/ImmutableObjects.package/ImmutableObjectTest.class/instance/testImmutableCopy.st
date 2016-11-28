testing
testImmutableCopy

	| objectUnderTest |
	objectUnderTest := self objectUnderTest immutableCopy.
	self assertPropertiesOf: objectUnderTest.
