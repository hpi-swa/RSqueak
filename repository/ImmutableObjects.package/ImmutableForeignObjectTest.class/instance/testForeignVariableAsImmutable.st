as yet unclassified
testForeignVariableAsImmutable

	| objectUnderTest |
	objectUnderTest := #( 'foo' (2) 'bar' nil ) asImmutable: ImmJustVariablePointersClass.
	self assert: 4 equals: objectUnderTest size.
	self assert: 'foo' equals: objectUnderTest first.
	self assert: #(2) equals: objectUnderTest second.
	self assert: 'bar' equals: objectUnderTest third.
	self assert: true equals:  objectUnderTest last.
	
