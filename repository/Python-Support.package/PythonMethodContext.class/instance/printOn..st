as yet unclassified
printOn: aStream
	| line lineno filename currentPath |
	self pyFrame ifNil: [
		aStream nextPutAll: 'Unknown pyFrame'.
		^ self ].
	line := PythonDebugger getSignature: pyFrame f_code.
	lineno := pyFrame f_lineno asSmalltalk.
	filename := pyFrame f_code co_filename asSmalltalk.
	currentPath := FileDirectory default pathName.
	(filename startsWith: currentPath) ifTrue: [
		filename := filename allButFirst: currentPath size + 1].
	aStream nextPutAll: line, ' (line ' , lineno asString, ' in ', filename, ')'