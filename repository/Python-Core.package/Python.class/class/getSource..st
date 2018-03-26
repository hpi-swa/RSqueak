source code
getSource: pyFrame
	| pyCode filename contents |
	pyCode := pyFrame f_code.
	filename := pyCode co_filename asSmalltalk.
	contents := (FileStream isAFileNamed: filename)
		ifTrue: [ self getContentsOf: filename ]
		ifFalse: [ pyCode co_source asSmalltalk ifNil: [ 'unknown' ] ].
	^ self filterPySource: contents lineno: pyCode co_firstlineno asSmalltalk