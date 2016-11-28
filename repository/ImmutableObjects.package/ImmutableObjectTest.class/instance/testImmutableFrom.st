testing
testImmutableFrom

	| objectUnderTest |
	objectUnderTest := ImmJustFixedPointersClass immutableFrom: (
		ImmJustFixedPointersClass new
			aField: 'foo';
			anotherField: #(2);
			someVar: 'bar' someOtherVar: true).

	self assert: 'foo' equals: objectUnderTest aField.
	self assert: #(2) equals: objectUnderTest anotherField.
	self assert: 'bar' equals: objectUnderTest someVar1.
	self assert: true equals:  objectUnderTest someVar2