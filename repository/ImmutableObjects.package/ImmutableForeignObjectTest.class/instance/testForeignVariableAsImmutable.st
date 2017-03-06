testing
testForeignVariableAsImmutable

	| objectUnderTest |
	objectUnderTest := #( 'foo' (2) 'bar' true ) asImmutable: ImmJustVariablePointersClass.
	self assert: 4 equals: objectUnderTest size.
	self assert: 'foo' equals: (objectUnderTest at: 1).
	self assert: #(2) equals: (objectUnderTest at: 2).
	self assert: 'bar' equals: (objectUnderTest at: 3).
	self assert: true equals: (objectUnderTest at: 4).
	
