instance creation
with: anObject
	"Answer a new instance of me, containing only anObject."
	<primitive: 'primitiveImmutableFromArgs' module: 'ImmutabilityPlugin'>
	" Fallback for non-immutable "
	^ super with: anObject