overrides
printOn: aStream
	| line lineno filename currentPath |
	self foreignFrame ifNil: [
		aStream nextPutAll: 'Unknown pyFrame'.
		^ self ].
	line := ForeignLanguage getSignature: foreignFrame f_code.
	lineno := foreignFrame f_lineno asSmalltalk.
	filename := foreignFrame f_code co_filename asSmalltalk.
	currentPath := FileDirectory default pathName.
	(filename startsWith: currentPath) ifTrue: [
		filename := filename allButFirst: currentPath size + 1].
	aStream nextPutAll: line, ' (line ' , lineno asString, ' in ', filename, ')'