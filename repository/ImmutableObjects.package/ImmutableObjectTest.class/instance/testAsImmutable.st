testing
testAsImmutable

	| objectUnderTest |
	objectUnderTest := self objectUnderTest asImmutable: self classUnderTest.
	self assertPropertiesOf: objectUnderTest.
