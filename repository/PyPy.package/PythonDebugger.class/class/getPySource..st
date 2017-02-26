pysource
getPySource: aContext
	| pyCode filename contents |
	pyCode := aContext pyFrame f_code.
	filename := pyCode co_filename.
	filename = '<string>' ifTrue: [ ^ 'Unable to get source.' ].
	contents := self getContentsOf: pyCode co_filename.
	^ self filterPySource: contents lineno: pyCode co_firstlineno
	