instance creation
val: anObject

	<primitive: 'primitiveValueFromArgs' module: 'ValuePlugin'>
	" Fallback for non-immutable "

	^ self basicNew
		instVarNamed: 'val' put: anObject;
		initialize