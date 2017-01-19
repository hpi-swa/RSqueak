as yet unclassified
withAll3: anArray
	| id |
	id := 'inst', PythonObject nextID asString.
	Python exec: id, ' = ', self name asString, '(', (anArray joinSeparatedBy: ',') , ')'.
	^ self basicNew initialize pyID: id; yourself