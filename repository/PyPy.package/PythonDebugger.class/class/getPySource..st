pysource
getPySource: aContext
	| pyCode contents |
	pyCode := aContext pyFrame f_code.
	contents := self getContentsOf: pyCode co_filename.
	^ self filterPySource: contents lineno: pyCode co_firstlineno
	