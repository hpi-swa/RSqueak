pysource
getSignature: pyCode
	| filename content |
	filename := pyCode co_filename.
	filename = '<string>' ifTrue: [ ^ 'unknown' ].
	content := self getContentsOf: pyCode co_filename.
	^ content lines at: pyCode co_firstlineno