pysource
getSignature: pyCode
	| content |
	content := self getContentsOf: pyCode co_filename.
	^ content lines at: pyCode co_firstlineno