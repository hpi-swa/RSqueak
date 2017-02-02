system primitives
resumeFrame
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	self primLastError ifNotNil: [ :e | self openDebuggerWithPythonFrames: e ].
	self primResume