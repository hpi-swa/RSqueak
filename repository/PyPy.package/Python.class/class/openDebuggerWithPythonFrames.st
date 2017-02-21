system primitives
openDebuggerWithPythonFrames
	self primLastError
		ifNil: [ self error: 'No Python error detected' ]
		ifNotNil: [ :e | self openDebuggerWithPythonFrames: e ]
	