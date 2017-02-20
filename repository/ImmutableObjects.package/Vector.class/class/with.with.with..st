instance creation
with: firstObject with: secondObject with: thirdObject
	"Answer a new instance of me, containing only the three arguments as
	elements."

	<primitive: 'primitiveImmutableFromArgs' module: 'ImmutabilityPlugin'>
	" Fallback for non-immutable "
	^ super with: firstObject with: secondObject with: thirdObject