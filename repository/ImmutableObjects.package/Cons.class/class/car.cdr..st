instance creation
car: anObject cdr: anotherObject

	<primitive: 'primitiveImmutableFromArgs' module: 'ImmutabilityPlugin'>
	" Fallback for non-immutable "

	^ self basicNew
		instVarNamed: 'car' put: anObject;
		instVarNamed: 'cdr' put: anotherObject;
		initialize