debugging
getSignature: pyCode
	| filename content |
	filename := pyCode co_filename asSmalltalk.
	filename = '<string>' ifTrue: [ ^ 'unknown' ].
	content := self getContentsOf: pyCode co_filename asSmalltalk.
	^ content lines at: pyCode co_firstlineno asSmalltalk