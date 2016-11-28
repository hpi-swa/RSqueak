testing
testAsImmutable

	| objectUnderTest |
	objectUnderTest := (ImmJustFixedPointersClass new
			aField: 'foo';
			anotherField: #(2);
			someVar: 'bar' someOtherVar: true)
		asImmutable: ImmJustFixedPointersClass.

	self assert: 'foo' equals: objectUnderTest aField.
	self assert: #(2) equals: objectUnderTest anotherField.
	self assert: 'bar' equals: objectUnderTest someVar1.
	self assert: true equals:  objectUnderTest someVar2.
