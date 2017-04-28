debugging
debuggerPrintItem: rbFrame on: aStream
	| name lineno filename currentPath |
	name := rbFrame get_code_name asSmalltalk.
	lineno := rbFrame get_lineno asSmalltalk asString.
	filename := rbFrame get_filename asSmalltalk.
	currentPath := FileDirectory default pathName.
	(filename startsWith: currentPath) ifTrue: [
		filename := filename allButFirst: currentPath size + 1].
	aStream nextPutAll: name, ' (line ' , lineno, ' in ', filename, ')'