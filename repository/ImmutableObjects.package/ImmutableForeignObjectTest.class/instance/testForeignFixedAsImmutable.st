as yet unclassified
testForeignFixedAsImmutable

	| objectUnderTest |
	objectUnderTest := #( 'foo' (2) 'bar' true ) asImmutable: ImmJustFixedPointersClass.
	self assert: 'foo' equals: objectUnderTest aField.
	self assert: #(2) equals: objectUnderTest anotherField.
	self assert: 'bar' equals: objectUnderTest someVar1.
	self assert: true equals:  objectUnderTest someVar2.
