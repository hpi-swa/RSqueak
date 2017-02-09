instance creation
with: firstObject with: secondObject 
	"Answer a new instance of me, containing firstObject and secondObject."

	<primitive: 'primitiveImmutableFromArgs' module: 'ImmutabilityPlugin'>
	" Fallback for non-immutable "
	^ super with: firstObject with: secondObject 