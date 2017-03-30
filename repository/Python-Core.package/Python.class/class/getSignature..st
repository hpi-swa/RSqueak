debugging
getSignature: pyCode
	| filename content |
	filename := pyCode co_filename asSmalltalk.
	(FileStream isAFileNamed: filename)
		ifFalse: [ ^ pyCode co_source asSmalltalk
			ifNotNil: [ :s | s lines first ]
			ifNil: [ 'unknown' ]].
	content := self getContentsOf: pyCode co_filename asSmalltalk.
	^ content lines at: pyCode co_firstlineno asSmalltalk