helpers
debuggerTitle: pyError
	| error |
	error := pyError asSmalltalk.
	^ error first, ': ', error second