instance creation
left: anObject val: anotherObject right: yetAnotherObject

	<primitive: 'primitiveValueFromArgs' module: 'ValuePlugin'>
	" Fallback for non-immutable "

	^ self basicNew
		instVarNamed: 'left' put: anObject;
		instVarNamed: 'val' put: anotherObject;
		instVarNamed: 'right' put: yetAnotherObject;
		initialize