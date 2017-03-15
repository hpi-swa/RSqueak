pySource
getPySource: aContext
	| pyCode filename contents |
	pyCode := aContext foreignFrame f_code.
	filename := pyCode co_filename asSmalltalk.
	filename = '<string>' ifTrue: [ ^ 'Unable to get source.' ].
	contents := self getContentsOf: filename.
	^ self filterPySource: contents lineno: pyCode co_firstlineno asSmalltalk
	