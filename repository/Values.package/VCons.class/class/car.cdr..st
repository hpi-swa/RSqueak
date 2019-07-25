instance creation
car: anObject cdr: anotherObject

	<primitive: 'primitiveValueFromArgs' module: 'ValuePlugin'>
	" Fallback for non-immutable "

	^ self basicNew
		instVarNamed: 'car' put: anObject;
		instVarNamed: 'cdr' put: anotherObject;
		initialize