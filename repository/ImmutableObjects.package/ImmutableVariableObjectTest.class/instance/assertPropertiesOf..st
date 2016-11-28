test objects
assertPropertiesOf: anImmutableObject

	self assert: 'foo' equals: (anImmutableObject at: 1).
	self assert: #(2) equals: (anImmutableObject at: 2).
	self assert: true equals: (anImmutableObject at: 3).
	self assert: nil equals: (anImmutableObject at: 4).