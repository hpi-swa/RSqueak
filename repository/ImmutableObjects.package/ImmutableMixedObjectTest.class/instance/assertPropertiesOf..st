test objects
assertPropertiesOf: anImmutableObject

	self assert: 'foo' equals: anImmutableObject aField.
	self assert: #(2) equals: anImmutableObject anotherField.
	self assert: true equals: (anImmutableObject at: 1).
	self assert: {} equals:  (anImmutableObject at: 2).
	self assert: 'bar' equals:  (anImmutableObject at: 3).