test objects
assertPropertiesOf: anImmutableObject

	(1 to: 10) do: [:i |
		self assert: (i * 2) equals: (anImmutableObject at: i)].