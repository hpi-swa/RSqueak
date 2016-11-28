testing
testAsImmutable

	| objectUnderTest |
	objectUnderTest := self objectUnderTest asImmutable: self classUnderTest.
	self attemptToModify: objectUnderTest.
	self assertPropertiesOf: objectUnderTest.
