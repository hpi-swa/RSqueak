instance creation
primValueFrom: aCollection
	" Overridden to be co-usable in mutable form "
	<primitive: 'primitiveValueFrom' module: 'ValuePlugin'>
	aCollection class isVariable ifFalse: [^ self primitiveFailed].
	
	^ (self basicNew: aCollection size)
		replaceFrom: 1 to: aCollection size with: aCollection