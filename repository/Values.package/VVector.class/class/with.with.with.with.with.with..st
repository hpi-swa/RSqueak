instance creation
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject
	"Answer a new instance of me, containing only the 6 arguments as elements."

	<primitive: 'primitiveValueFromArgs' module: 'ValuePlugin'>
	" Fallback for non-immutable "
	^ super with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject
