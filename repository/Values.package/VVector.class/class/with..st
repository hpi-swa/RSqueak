instance creation
with: anObject
	"Answer a new instance of me, containing only anObject."
	<primitive: 'primitiveValueFromArgs' module: 'ValuePlugin'>
	" Fallback for non-immutable "
	^ super with: anObject