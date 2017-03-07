special methods
resumeFrame
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	[ ^ self primResume ] on: Error do: [
		self primLastError ifNotNil: [ :e | self openDebuggerWithPythonFrames: e ].
		^ self resumeFrame ]