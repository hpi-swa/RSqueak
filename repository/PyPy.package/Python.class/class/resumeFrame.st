special methods
resumeFrame
	"Magic method that is called by vm (cached in vm)"
	Processor yield.
	[ ^ self primResume ] on: Error do: [
		self openDebuggerWithPythonFrames.
		^ self resumeFrame ]