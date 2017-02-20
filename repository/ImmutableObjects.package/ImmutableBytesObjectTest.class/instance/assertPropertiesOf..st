test objects
assertPropertiesOf: anImmutableObject

	self assert: $T equals: anImmutableObject first.
	self assert: 'This is a test string!' equals: anImmutableObject