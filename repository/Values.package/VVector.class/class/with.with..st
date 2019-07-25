instance creation
with: firstObject with: secondObject 
	"Answer a new instance of me, containing firstObject and secondObject."

	<primitive: 'primitiveValueFromArgs' module: 'ValuePlugin'>
	" Fallback for non-immutable "
	^ super with: firstObject with: secondObject 