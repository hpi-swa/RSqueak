test objects
assertPropertiesOf: anImmutableObject

	self assert: 'foo' equals: anImmutableObject first.
	self assert: #(2) equals: anImmutableObject second.
	self assert: true equals: anImmutableObject third.
	self assert: nil equals:  anImmutableObject last.