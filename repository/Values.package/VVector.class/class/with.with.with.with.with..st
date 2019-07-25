instance creation
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
	"Answer a new instance of me, containing only the five arguments as
	elements."

	<primitive: 'primitiveValueFromArgs' module: 'ValuePlugin'>
	" Fallback for non-immutable "
	^ super with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
