*ImmutableObjects
newWith: selectorArray and: argumentArray
	<primitive: 'newImmutableObject' module: 'ImmutabilityPlugin'>
	| obj |
	obj := self basicNew.
	selectorArray doWithIndex: [ :selector :argsIdx | | args |
		args := argumentArray at: argsIdx.
		obj perform: selector withArguments: args ].
	^ obj