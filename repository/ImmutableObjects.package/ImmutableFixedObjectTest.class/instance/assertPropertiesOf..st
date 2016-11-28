test objects
assertPropertiesOf: anImmutableObject

	self assert: 'foo' equals: anImmutableObject aField.
	self assert: #(2) equals: anImmutableObject anotherField.
	self assert: 'bar' equals: anImmutableObject someVar1.
	self assert: true equals:  anImmutableObject someVar2.