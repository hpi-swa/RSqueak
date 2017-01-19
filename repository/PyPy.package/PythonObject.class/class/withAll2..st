as yet unclassified
withAll2: anArray
	| id obj |
	id := 'inst', PythonObject nextID asString.
	Python exec: id, ' = ', self name asString, '(', (anArray joinSeparatedBy: ',') , ')'.
	obj := Python eval: id.
	obj pyID: id.
	^ obj