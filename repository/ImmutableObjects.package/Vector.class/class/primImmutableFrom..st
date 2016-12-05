instance creation
primImmutableFrom: aCollection
	" Overridden to be co-usable in mutable form "
	<primitive: 'primitiveImmutableFrom' module: 'ImmutabilityPlugin'>
	aCollection class isVariable ifFalse: [^ self primitiveFailed].
	
	^ (self basicNew: aCollection size)
		replaceFrom: 1 to: aCollection size with: aCollection